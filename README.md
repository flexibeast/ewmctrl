# ewmctrl - Use `wmctrl` to manage desktop windows

[![Join the chat at https://gitter.im/flexibeast/ewmctrl](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/flexibeast/ewmctrl?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

*Author:* Alexis <flexibeast@gmail.com><br>

`ewmctrl` provides an Emacs interface to the `wmctrl` command-line window-management program.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Issues](#issues)
- [License](#license)

## Installation

Install [ewmctrl from MELPA](http://melpa.org/#/ewmctrl), or put `ewmctrl.el` in your load-path and do a `(require 'ewmctrl)`.

## Usage

Create an `ewmctrl` buffer with <kbd>M-x ewmctrl</kbd>.

The default keybindings are:

### Window actions

* RET - Switch to the selected desktop window (`ewmctrl-focus-window`).

* D - Delete the selected desktop window (`ewmctrl-delete-window`).

* I - Change the icon name of the selected desktop window (`ewmctrl-change-window-icon-name`).

* m - Move the selected desktop window to a different desktop (`ewmctrl-move-window-to-other-desktop`).

* M - Move the selected desktop window to the current desktop, raise it, and give it focus (`ewmctrl-move-window-to-current-desktop-and-focus`).

* N - Change the name of the selected desktop window (`ewmctrl-change-window-name`).

* r - Resize the selected desktop window by specifying dimensions in the minibuffer (`ewmctrl-resize-window`). Whilst in the minibuffer, use TAB and S-TAB to move within and between the width and height fields, and use C-RET to preview the currently specified dimensions.

### Filtering

* fc - Remove all filtering (`ewmctrl-filters-clear`).

* fd - Add a filter by desktop number (`ewmctrl-filter-by-desktop-number`).

* fD - Remove all filtering by desktop number (`ewmctrl-filter-desktop-number-clear`).

* fn - Add a filter by window name (`ewmctrl-filter-by-name`).

* fN - Remove all filtering by window name (`ewmctrl-filter-name-clear`).

* fp - Add a filter by PID (`ewmctrl-filter-by-pid`).

* fP - Remove all filtering by PID (`ewmctrl-filter-pid-clear`).

### Sorting

* Sd - Sort the list of desktop windows numerically by desktop number (`ewmctrl-sort-by-desktop-number`).

* SD - Sort the list of desktop windows reverse-numerically by desktop number (`ewmctrl-sort-by-desktop-number-reversed`).

* Sn - Sort the list of desktop windows lexicographically by name (`ewmctrl-sort-by-name`).

* SN - Sort the list of desktop windows reverse-lexicographically by name (`ewmctrl-sort-by-name-reversed`).

* Sp - Sort the list of desktop windows numerically by PID (`ewmctrl-sort-by-pid`).

* SP - Sort the list of desktop windows reverse-numercially by PID (`ewmctrl-sort-by-pid-reversed`).

### General

* g - Refresh the list of desktop windows (`ewmctrl-refresh`).

* n - Move point to next line (`next-line`).

* p - Move point to previous line (`previous-line`).

Customisation options are available via the `ewmctrl` customize-group.

## Issues / bugs

Deletion of windows does not work in i3 4.8 and earlier due to [i3 bug #1396](http://bugs.i3wm.org/query/ticket/1396).

If you discover an issue or bug in `ewmctrl` not already noted:

* as a TODO item, or

* in [the project's "Issues" section on GitHub](https://github.com/flexibeast/ewmctrl/issues),

please create a new issue with as much detail as possible, including:

* which version of Emacs you're running on which operating system, and

* how you installed `ewmctrl`.

## License

[GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.


---
Converted from `ewmctrl.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
