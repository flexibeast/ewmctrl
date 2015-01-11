# ewmctrl - use `wmctrl` to manage desktop windows

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

The default keybindings, applicable to the desktop window selected by the line at point, are:

* RET - Switch to the selected desktop window (`ewmctrl-focus-window`).

* D - Delete the selected desktop window (`ewmctrl-delete-window`).

* g - Refresh the list of desktop windows (`ewmctrl-refresh`).

* i - Change the icon name of the selected desktop window (`ewmctrl-change-window-icon-name`).

* n - Change the name of the selected desktop window (`ewmctrl-change-window-name`).

* Sn - Sort the list of desktop windows lexicographically by name (`ewmctrl-sort-by-name`).

* SN - Sort the list of desktop windows reverse-lexicographically by name (`ewmctrl-sort-by-name-reversed`).

* Sp - Sort the list of desktop windows numerically by PID (`ewmctrl-sort-by-pid`).

* SP - Sort the list of desktop windows reverse-numercially by PID (`ewmctrl-sort-by-pid-reversed`).

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
