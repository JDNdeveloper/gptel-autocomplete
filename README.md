# gptel-autocomplete

`gptel-autocomplete` is an Emacs package that provides inline code completion using the [`gptel`](https://github.com/karthink/gptel) package. It displays AI-generated completions as ghost text at the cursor position, allowing you to preview and accept suggestions while coding.

## Features

- Request completions from LLMs.
- Display inline ghost text completions.
- Accept completions to insert them into your buffer.
- Overlay-only keymap for completion acceptance bindings.
- Optional idle-triggered automatic completion via minor mode.
- Includes context around the cursor (context window size is configurable).
- Provide additional context using standard `gptel` features (e.g. `gptel-add`).

## Requirements

- Emacs 27.2 or newer
- [`gptel`](https://github.com/karthink/gptel) package properly installed and configured

## Installation

First, follow the [gptel setup instructions](https://github.com/karthink/gptel) to install and configure `gptel`.

Then, install `gptel-autocomplete` using `straight.el` by adding the following to your Emacs config:

```elisp
(straight-use-package
 '(gptel-autocomplete :type git :host github :repo "JDNdeveloper/gptel-autocomplete"))
```

After installation, require and enable the package as needed:

```elisp
(require 'gptel-autocomplete)
```

## Configuration

You can customize the amount of context sent before and after the cursor:

```elisp
(setq gptel-autocomplete-before-context-lines 100)
(setq gptel-autocomplete-after-context-lines 20)
```

Set a custom temperature (lower values tend to yield better results):

```elisp
(setq gptel-autocomplete-temperature 0.1)
```

Enable debug messages if you want detailed logs:

```elisp
(setq gptel-autocomplete-debug t)
```

Control whether gptel's context system is used in completions (enabled by default):

```elisp
;; Enable context inclusion.
(setq gptel-autocomplete-use-context t)

;; Disable context inclusion.
(setq gptel-autocomplete-use-context nil)
```

Configure automatic idle completion:

```elisp
;; Wait 0.3 seconds before auto-completing when idle.
(setq gptel-autocomplete-idle-delay 0.3)

;; Trigger immediately after a command when Emacs becomes idle.
(setq gptel-autocomplete-idle-delay 0)

;; Disable idle completion.
(setq gptel-autocomplete-idle-delay nil)
```

Bind keys that are active only while ghost text is visible:

```elisp
;; Example: use C-e to accept completion only when overlay is visible.
(keymap-set gptel-autocomplete-completion-map "C-e" #'gptel-accept-completion)

;; Optional: accept one word at a time.
(keymap-set gptel-autocomplete-completion-map "M-f" #'gptel-accept-word)
```

## Usage

- `M-x gptel-complete` — Request a completion at point and display it as ghost text.
- `M-x gptel-accept-completion` — Accept the currently displayed completion and insert it.
- `M-x gptel-accept-word` — Accept only one word of the completion, keeping ghost text for the rest.
- `M-x gptel-autocomplete-mode` — Enable or disable idle-triggered automatic completion in the current buffer.

You can bind these commands to convenient keys in your preferred programming modes.

## Appendix

### AI use disclaimer

`gptel-autocomplete.el` was mostly written by Claude Sonnet 4.

### Which LLMs work best?

I've tested it on gpt-4.1-mini, Qwen3 14B, and Devstral Small 2505. It's worked decently well on all three, though as you'd expect the higher end models tend to do better. Devstral Small 2505, and coding-optimized models in general, struggle sometimes to follow the system prompt and occasionally return responses in the wrong format.

### Getting chat LLMs to generate code completions

`gptel` uses chat completion requests which aren't a natural fit for inline code completion. Simple prompts yield poor results, for example:

System prompt:
```
Complete the code provided by the user. Only include the text that logically follows.

NEVER repeat back the users' input.
```

User prompt:
```
function calculateArea(width: number, height: number): number {
  return width * 
```

This would typically return something like:

````
```typescript
function calculateArea(width: number, height: number): number {
  return width * area;
}
```
````

When what I wanted is:

```
area;
```

Some common issues encountered in responses:
- Included backtick code blocks.
- Included extra characters that conflicted with following lines (e.g. closing brackets).
- Included prior lines from the input rather than just net-new completion text.

The approach I took was to lean into the way the chat models want to produce output, and then post process the response to extract the completion.

I found the following techniques yielded good results:
- Ask for the response to be contained within backtick code blocks. Chat models *really* don't like returning code on its own.
- Have the response include the current line being completed (this is then stripped out in post-processing). Chat models *really* don't like sending incomplete lines of code.
- Include markers surrounding the line to be completed, to make it clear to the model that code should not be generated outside of this section. Also ask for the reponse to include these markers, which helps reinforce that this is the only region that should be modified.
- Include an explicit cursor marker.
- Provide explicit examples of bad output in the system prompt.
- Use a lower temperature for more deterministic responses.

Here's an example of how code is sent in the user prompt:

```
function foo(a, b) {
█START_COMPLETION█
    if (a < b) █CURSOR█
█END_COMPLETION█
}
```

Some things I tried that did NOT work:
- Using FIM tokens (e.g. `<|fim_suffix|>`). Most chat models do not seem to follow FIM-style prompts well. Backticks and custom markers seemed to work better.
- Placing the after-context at the beginning of the prompt (I was inspired to try this by [Minuet](https://github.com/milanglacier/minuet-ai.el/blob/main/prompt.md)). I found that the response would usually repeat back part of the after-context when prompted in this way, and it was less prone to doing this when the completion region was included inline.
