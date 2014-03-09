# Manage your minor-mode on the dedicated interface buffer

![manage-minor-mode](https://raw2.github.com/ShingoFukuyama/images/master/manage-minor-mode.png)

## Usage

1. M-x manage-minor-mode
2. Minor modes list buffer will appear in other window
3. Then type "RET" key on any minor-mode you want to toggle active/inactive

## Mouse menu

![manage-minor-mode-menu](https://raw2.github.com/ShingoFukuyama/images/master/manage-minor-mode-menu.png)

You can click minor-mode indicated in the mode-line to pop up manage-minor-mode menu item.

## Eradicate all minor-modes

M-x `manage-minor-mode-bals` to disable all minor-modes.  
M-x `manage-minor-mode-restore-from-bals` to restore the minor-modes before `manage-minor-mode-bals` done.  
It might be useful when you view a huge size file smoothly.
