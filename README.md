# Manage your minor-mode on the dedicated interface buffer

![manage-minor-mode](https://raw2.github.com/ShingoFukuyama/images/master/manage-minor-mode.png)

## Usage

1. M-x manage-minor-mode
2. Minor modes list buffer will appear in other window
3. Then type "RET" key on any minor-mode you want to toggle active/inactive

## Mouse menu

You can click minor-mode indicated in the mode-line to pop up manage-minor-mode menu item.

![manage-minor-mode-menu](https://raw2.github.com/ShingoFukuyama/images/master/manage-minor-mode-menu.png)

## Set minor-modes status for each major-mode in advance

```
(setq manage-minor-mode-default
      '((emacs-lisp-mode
         (on  rainbow-delimiters-mode eldoc-mode show-paren-mode)
         (off line-number-mode))
        (js2-mode
         (on  color-identifiers-mode)
         (off line-number-mode))))
```

## Eradicate all minor-modes

M-x `manage-minor-mode-bals` to disable all minor-modes.  
M-x `manage-minor-mode-restore-from-bals` to restore the minor-modes before `manage-minor-mode-bals` done.  
It might be useful when you view a huge size file smoothly.

List minor-modes that exception from `manage-minor-mode-bals` like below.

```
(setq manage-minor-mode-bals-exclude-list
      '((global (recentf-mode global-font-lock-mode delete-selection-mode transient-mark-mode tabbar-mode))
        (text-mode (line-number-mode))
        (org-mode (line-number-mode blink-cursor-mode))))
```

