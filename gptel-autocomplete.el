;;; gptel-autocomplete.el --- Autocomplete support for gptel -*- lexical-binding: t -*-

;; Author: Jayden Navarro
;; Version: 0.1
;; Package-Requires: ((emacs "27.2") (gptel "20250524.720"))
;; Keywords: convenience, completion, gptel, copilot, agent, ai

;;; Commentary:
;;
;; Provides `gptel-complete` and `gptel-accept-completion` to
;; request and display inline completions from ChatGPT via `gptel-request`.
;; Includes debug instrumentation when `gptel-autocomplete-debug` is non-nil.
;;
;;; Code:

(require 'subr-x)
(require 'gptel)
(require 'cl-lib)

(defgroup gptel-autocomplete nil
  "Inline completion support for gptel."
  :group 'convenience)

(defcustom gptel-autocomplete-debug nil
  "Non-nil enables debug messages in gptel-autocomplete."
  :type 'boolean
  :group 'gptel-autocomplete)

(defcustom gptel-autocomplete-before-context-lines 100
  "Number of characters to include before the cursor for context.
A larger value provides more context but may hit token limits."
  :type 'integer
  :group 'gptel-autocomplete)

(defcustom gptel-autocomplete-after-context-lines 20
  "Number of characters to include after the cursor for context.
A smaller value is usually sufficient since the model primarily
needs to understand what comes before the cursor."
  :type 'integer
  :group 'gptel-autocomplete)

(defcustom gptel-autocomplete-temperature 0.1
  "Temperature to use for code‐completion requests in `gptel-complete`.
This value will override `gptel-temperature` when calling `gptel-complete`."
  :type 'number
  :group 'gptel-autocomplete)

(defcustom gptel-autocomplete-use-context t
  "Whether to include gptel context in autocomplete requests.
When non-nil, gptel's context system (gptel-context) will be used
to include additional context in completion requests. This allows
the AI to consider related files or marked regions when generating
completions."
  :type 'boolean
  :group 'gptel-autocomplete)

(defcustom gptel-autocomplete-idle-delay nil
  "Time in seconds to wait before starting automatic completion.
Complete immediately if set to 0.
Disable idle completion if set to nil."
  :type '(choice
          (number :tag "Seconds of delay")
          (const :tag "Idle completion disabled" nil))
  :group 'gptel-autocomplete)

(defvar gptel--completion-text nil
  "Current GPTel completion text.")

(defvar gptel--completion-overlay nil
  "Overlay for displaying GPTel completion ghost text.")

(defvar-local gptel--completion-keymap-overlay nil
  "Overlay used to activate `gptel-autocomplete-completion-map'.")

(defvar gptel--completion-overlays nil
  "List of all GPTel completion overlays for cleanup.")

(defvar gptel--completion-request-id 0
  "Counter for tracking completion requests.")

(defvar gptel--completion-suppress-clear nil
  "Non-nil suppresses the next post-command clear.")

(defvar-local gptel--post-command-timer nil
  "Idle timer used to debounce automatic completion requests.")

(defvar-local gptel--completion-active-request-id nil
  "Request id for the current in-flight completion request.")

(defconst gptel-autocomplete-completion-map (make-sparse-keymap)
  "Keymap active only while a completion overlay is visible.")

(defvar gptel-autocomplete-mode-map (make-sparse-keymap)
  "Keymap for `gptel-autocomplete-mode'.")

(define-minor-mode gptel-autocomplete-mode
  "Toggle automatic idle completions in the current buffer."
  :init-value nil
  :lighter " GPTel-A"
  :keymap gptel-autocomplete-mode-map
  (if gptel-autocomplete-mode
      (add-hook 'post-command-hook #'gptel--post-command nil 'local)
    (remove-hook 'post-command-hook #'gptel--post-command 'local)
    (gptel--cancel-post-command-timer)
    (gptel-clear-completion)))

(defun gptel--get-or-create-keymap-overlay ()
  "Return the local keymap overlay for completion bindings."
  (unless (overlayp gptel--completion-keymap-overlay)
    (setq gptel--completion-keymap-overlay (make-overlay 1 1 nil nil t))
    (push gptel--completion-keymap-overlay gptel--completion-overlays)
    (overlay-put gptel--completion-keymap-overlay
                 'keymap gptel-autocomplete-completion-map)
    (overlay-put gptel--completion-keymap-overlay 'priority 1001))
  gptel--completion-keymap-overlay)

(defun gptel--move-keymap-overlay-at-point (&optional position)
  "Move the completion keymap overlay to POSITION.
If POSITION is nil, use point."
  (let ((ov (gptel--get-or-create-keymap-overlay)))
    (save-excursion
      (when position
        (goto-char position))
      (move-overlay ov (point) (min (point-max) (+ 1 (point)))))))

(defun gptel--cancel-post-command-timer ()
  "Cancel the local post-command idle timer, if any."
  (when gptel--post-command-timer
    (cancel-timer gptel--post-command-timer)
    (setq gptel--post-command-timer nil)))

(defun gptel--post-command-debounce (buffer point tick)
  "Request completion in BUFFER if POINT and TICK are unchanged."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
             gptel-autocomplete-mode
             (numberp gptel-autocomplete-idle-delay)
             (>= gptel-autocomplete-idle-delay 0)
             (= (point) point)
             (= (buffer-chars-modified-tick) tick)
             (eolp)
             (not gptel--completion-overlay))
    (gptel-complete)))

(defun gptel--post-command ()
  "Schedule completion in `post-command-hook' when idle completion is enabled."
  (gptel--cancel-post-command-timer)
  (when (and (numberp gptel-autocomplete-idle-delay)
             (>= gptel-autocomplete-idle-delay 0)
             (not (minibufferp))
             (not (active-minibuffer-window))
             (eolp)
             (not gptel--completion-overlay))
    (setq gptel--post-command-timer
          (run-with-idle-timer gptel-autocomplete-idle-delay
                               nil
                               #'gptel--post-command-debounce
                               (current-buffer)
                               (point)
                               (buffer-chars-modified-tick)))))

(defun gptel--completion-allowed-p ()
  "Return non-nil when completion should run at point."
  (and (not (minibufferp))
       (eolp)))

(defun gptel--abort-supported-p ()
  "Return non-nil when request abort is supported for completions."
  (and (fboundp 'gptel-abort)
       (boundp 'gptel-use-curl)
       (or (and (stringp gptel-use-curl)
                (file-executable-p gptel-use-curl))
           (and gptel-use-curl
                (executable-find "curl")))))

(defun gptel--cancel-active-completion-request ()
  "Cancel an active completion request when transport supports abort."
  (when (and gptel--completion-active-request-id
             (gptel--abort-supported-p))
    (gptel--log "Canceling in-flight completion request")
    (let ((inhibit-message t)
          (message-log-max nil))
      (gptel-abort (current-buffer)))))

(defun gptel--log (fmt &rest args)
  "Log message FMT with ARGS if `gptel-autocomplete-debug` is non-nil."
  (when gptel-autocomplete-debug
    (apply #'message (concat "[gptel-autocomplete] " fmt) args)))

(defun gptel-clear-completion ()
  "Clear all GPTel completion overlays and text."
  (gptel--log "Clearing completion overlays/text")
  ;; Clear the main overlay
  (when gptel--completion-overlay
    (delete-overlay gptel--completion-overlay)
    (setq gptel--completion-overlay nil))
  ;; Clear all tracked overlays
  (dolist (ov gptel--completion-overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (when (overlayp gptel--completion-keymap-overlay)
    (delete-overlay gptel--completion-keymap-overlay)
    (setq gptel--completion-keymap-overlay nil))
  (setq gptel--completion-overlays nil)
  (setq gptel--completion-text nil)
  (setq gptel--completion-suppress-clear nil)
  (remove-hook 'post-command-hook #'gptel--post-command-clear t))

(defun gptel--post-command-clear ()
  "Clear ghost text unless suppressed for this command."
  (if gptel--completion-suppress-clear
      (setq gptel--completion-suppress-clear nil)
    (gptel-clear-completion)))

(defun gptel--setup-ghost-clear-hook ()
  "Set up hook to clear ghost text on user interaction."
  (add-hook 'post-command-hook #'gptel--post-command-clear nil t))

;;;###autoload
(defun gptel-complete ()
  "Request a completion from ChatGPT and display it as ghost text."
  (interactive)
  (when (gptel--completion-allowed-p)
    (gptel--cancel-post-command-timer)
    (gptel-clear-completion)
    (gptel--cancel-active-completion-request)
    (let* ((gptel-temperature gptel-autocomplete-temperature)
           (filename (if (buffer-file-name)
                         (file-name-nondirectory (buffer-file-name))
                       (buffer-name)))
           (line-start (line-beginning-position))
           (line-end (line-end-position))
           (cursor-pos-in-line (- (point) line-start))
           (current-line (buffer-substring-no-properties line-start line-end))
           (before-cursor-in-line (substring current-line 0 cursor-pos-in-line))
           (after-cursor-in-line (substring current-line cursor-pos-in-line))
           (before-start (max (point-min)
                              (save-excursion
                                (forward-line (- gptel-autocomplete-before-context-lines))
                                (line-beginning-position))))
           (after-end (min (point-max)
                           (save-excursion
                             (goto-char line-end)
                             (forward-line gptel-autocomplete-after-context-lines)
                             (line-end-position))))
           (before-context (buffer-substring-no-properties before-start line-start))
           (after-context (buffer-substring-no-properties line-end after-end))
           ;; Construct the marked context with completion boundaries
           (marked-line (concat "█START_COMPLETION█\n"
                                before-cursor-in-line "█CURSOR█" after-cursor-in-line "\n"
                                "█END_COMPLETION█"))
           (context (concat before-context marked-line after-context))
           (prompt (concat "Complete the code at the cursor position █CURSOR█ in file '"
                           filename "':\n````````\n"
                           context "\n````````\n"))
           (request-id (cl-incf gptel--completion-request-id))
           (target-point (point)))
      (setq gptel--completion-active-request-id request-id)
      (gptel--log "Sending prompt of length %d (request-id: %d)"
                  (length prompt) request-id)
      (when gptel-autocomplete-debug
        (gptel--log "Full prompt:\n%s" prompt))
      (gptel-request
       prompt
       :system "/no_think
You are a code completion assistant integrated into a code editor.

Complete the code at the cursor position █CURSOR█. The █START_COMPLETION█ and █END_COMPLETION█ \
markers indicate the exact region where your completion should be inserted, and you should only \
complete code in that one specific region.

RESPONSE REQUIREMENTS:
1. Response should be contained within code triple backticks (```)
2. MUST start with █START_COMPLETION█ on its own line
3. MUST end with █END_COMPLETION█ on its own line
4. Complete the line containing █CURSOR█ (replacing █CURSOR█ with appropriate code)
5. Do NOT include any code that appears after █END_COMPLETION█ in the input text
6. Add any additional lines that logically follow between the markers
7. If only █CURSOR█ is present in the completion section, generate new lines that follow \
from the code before █START_COMPLETION█ (do NOT repeat the █CURSOR█ token)
8. Generate a MINIMAL response (between 1-20 lines, the shorter and higher-confidence the \
better; MOST of your responses will JUST BE ONE LINE)

Example input:
```
function foo(a, b) {
█START_COMPLETION█
    if (a < b) █CURSOR█
█END_COMPLETION█
}

function bar() {
    console.log('bar');
}
```

Example correct output:
```
█START_COMPLETION█
    if (a < b) {
        return a;
    }
    return b;
█END_COMPLETION█
```

Example WRONG output (do NOT do this; never provide output after end completion marker):
```
█START_COMPLETION█
    if (a < b) {
        return a;
    }
    return b;
█END_COMPLETION█
}

function bar() {
    console.log('bar');
}
```

Example input:
```
function foo(a, b) {
█START_COMPLETION█
    █CURSOR█
█END_COMPLETION█
}
```

Example correct output:
```
█START_COMPLETION█
    return a < b;
█END_COMPLETION█
```

Example WRONG output (do NOT do this; never repeat the cursor token):
```
█START_COMPLETION█
    █CURSOR█
█END_COMPLETION█
```
"
       :buffer (current-buffer)
       :position target-point
       :transforms (when gptel-autocomplete-use-context
                     gptel-prompt-transform-functions)
       :callback
       (lambda (response info)
         (when (eq request-id gptel--completion-active-request-id)
           (setq gptel--completion-active-request-id nil))
         (gptel--log "Callback invoked: status=%s, request-id=%d, current-id=%d, raw-response=%S"
                     (plist-get info :status) request-id
                     gptel--completion-request-id response)
         ;; Only process if this is still the latest request
         (if (not (eq request-id gptel--completion-request-id))
             (gptel--log "Ignoring outdated request %d (current: %d)"
                         request-id gptel--completion-request-id)
           (pcase response
             ((pred null)
              (message "gptel-complete failed: %s" (plist-get info :status)))
             (`abort
              (gptel--log "Request aborted"))
             (`(tool-call . ,tool-calls)
              (gptel--log "Ignoring tool-call response: %S" tool-calls))
             (`(tool-result . ,tool-results)
              (gptel--log "Ignoring tool-result response: %S" tool-results))
             (`(reasoning . ,text)
              (gptel--log "Ignoring reasoning block (thinking) response: %S" text))
             ((pred stringp)
              (let* ((trimmed (string-trim response))
                     ;; Extract code from markdown code blocks
                     (code-content (if (string-match
                                        "^```\\(?:[a-zA-Z]*\\)?\n\\(\\(?:.\\|\n\\)*?\\)\n```$"
                                        trimmed)
                                       (match-string 1 trimmed)
                                     trimmed))
                     ;; Extract content between START_COMPLETION and END_COMPLETION markers
                     (completion-text
                      (if (and code-content
                               (string-match
                                "█START_COMPLETION█\n\\(\\(?:.\\|\n\\)*?\\)\n█END_COMPLETION█"
                                code-content))
                          (let ((extracted (match-string 1 code-content)))
                            (gptel--log "Extracted completion between markers: %S" extracted)
                            ;; Remove the part before cursor on the current line
                            (if (and extracted before-cursor-in-line
                                     (not (string-empty-p before-cursor-in-line)))
                                (let ((lines (split-string extracted "\n" t))
                                      (first-line (car (split-string extracted "\n"))))
                                  (if (and first-line
                                           (string-prefix-p before-cursor-in-line first-line))
                                      (let ((remainder (substring
                                                        first-line
                                                        (length before-cursor-in-line))))
                                        (if (cdr lines)
                                            (concat remainder "\n" (string-join (cdr lines) "\n"))
                                          remainder))
                                    extracted))
                              extracted))
                        (progn
                          (gptel--log "No completion markers found, falling back to full response")
                          ;; Fallback to old logic if no markers found
                          (if (and code-content before-cursor-in-line
                                   (not (string-empty-p before-cursor-in-line)))
                              (let ((overlap-pos (string-search before-cursor-in-line code-content)))
                                (if overlap-pos
                                    (substring code-content
                                               (+ overlap-pos
                                                  (length before-cursor-in-line)))
                                  code-content))
                            code-content)))))
                (setq gptel--completion-text completion-text)
                (when (and completion-text (not (string-empty-p completion-text)))
                  (let ((ov (make-overlay target-point target-point)))
                    (setq gptel--completion-overlay ov)
                    (push ov gptel--completion-overlays)
                    (gptel--move-keymap-overlay-at-point target-point)
                    (overlay-put ov 'after-string
                                 (propertize completion-text
                                             'face 'shadow
                                             'cursor t))
                    (overlay-put ov 'priority 1000))
                  (gptel--setup-ghost-clear-hook)
                  (gptel--log "Displayed ghost text: %S" completion-text))))
             (_
              (gptel--log "Unexpected response type: %S" response)))))))))

;;;###autoload
(defun gptel-accept-completion ()
  "Accept the current GPTel completion, inserting it into the buffer."
  (interactive)
  (if (and gptel--completion-text (not (string-empty-p gptel--completion-text)))
      (progn
        (gptel--log "Accepting completion: %S" gptel--completion-text)
        ;; Don't use save-excursion here
        (insert gptel--completion-text)
        (gptel-clear-completion))
    (message "No completion to accept.")))

;;;###autoload
(defun gptel-accept-word ()
  "Accept the next word from the current GPTel completion."
  (interactive)
  (if (and gptel--completion-text (not (string-empty-p gptel--completion-text)))
      (let* ((text gptel--completion-text)
             (non-space-pos (string-match "[^[:space:]]" text))
             (end-pos (cond
                       ((not non-space-pos) (length text))
                       (t
                        (or (string-match "[[:space:]]" text non-space-pos)
                            (length text)))))
             (next-chunk (substring text 0 end-pos))
             (remainder (substring text end-pos)))
        (gptel--log "Accepting word chunk: %S" next-chunk)
        (insert next-chunk)
        (when (overlayp gptel--completion-overlay)
          (move-overlay gptel--completion-overlay (point) (point)))
        (gptel--move-keymap-overlay-at-point)
        (setq gptel--completion-suppress-clear t)
        (setq gptel--completion-text remainder)
        (if (and remainder (not (string-empty-p remainder)))
            (when (overlayp gptel--completion-overlay)
              (overlay-put gptel--completion-overlay 'after-string
                           (propertize remainder
                                       'face 'shadow
                                       'cursor t)))
          (gptel-clear-completion)))
    (message "No completion to accept.")))

(provide 'gptel-autocomplete)
;;; gptel-autocomplete.el ends here
