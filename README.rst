==============================================================================
Tab Based Indent -- Edit space-based indented files with tab-based indentation
==============================================================================

This package provides **tbindent-mode**, a minor mode which provides the
ability to transparently convert the content of a file that is indented with a
fixed number of spaces into a buffer which uses tab-based indentation and save
the buffer content back to file with the original space-based indentation
scheme.

Overview
========

**Why would you want to use this?**

You can use this minor mode if you'd like to see the indentation with a
different width while viewing or editing its content.  That may appeal to
people that have problems or do not like working with code that use a small
space-based indentation width.  With the rising popularity of space based
indentation with a 2 column indentation width, the need for a work-around
solution increases.

**How does it work?**

Given appropriate conditions, the **tbindent-mode** converts a buffer content
from space-based indentation to pure hard-tabs based indentation.

These conditions are simple:  the value of ``tab-width``must be equal to the
indentation value identified by the indentation control variable used for the
major-mode (such as ``c-basic-offset`` in C, C++ , D buffers, or
``ada-indent`` for Ada buffers, or ``python-indent-offset`` for Python
buffers).  Those values must also represent the real indentation width.

With those conditions met inside the buffer, an indentation step identified by
the indentation control variable corresponds to the tab width and we can now
change the tab-width and the value of the indentation control variable to
change the visual rendering of indentation to be narrower or wider.

- When **tbindent-mode** is turned on, it:

  - remembers the original indentation scheme,
  - converts each group of spaces that constitute one indentation step into
    one hard tab and adjust the value of several buffer local variables:

    - it sets both ``tab-width`` and the variable that controls indentation
      for the buffer's major-mode to the same value,  if the indentation width
      for the current major-mode is identified by the
      **tbindent-target-indent-widths** customizable user-option it changes
      these to that value, otherwise it keeps what is currently used,
    - it turns on the ``indent-tabs-mode``,
    - it manages the ``fill-column`` value (more on this later).

- When **tbindent-mode** is turned off, it

   - converts the content of the buffer back to the original space-based
     indentation scheme.

**Saving To File**

When **tbindent-mode** is active and you save the buffer back to its file, the
mode seamlessly converts the buffer content back to its original space-based
indentation scheme, stores the buffer content into the file and then restore
the tabs-based indentation to let you continue editing the file.

**Undoing**

These conversions from space-based indentation to tabs-based are excluded from
the buffer's undo list and do not show up as a buffer modification.  It's
really the case, your conversions are just a visual rendering change, the file
is not modified.

**Dynamically changing the indentation width**

While **tbindent-mode** is on, use the **tbindent-set-tab-width** to change
the indentation width.  That command changes the buffer local value of
``tab-width`` and the indentation control variable used by the major mode.
With that command, you can quickly widen or narrow the indentation width in
the buffer.  This modification does **not** and will not affect the content of
the file.

**What about auto-fill mode?**

While **tbindent-mode** is on, a special function is used to control the value
of ``fill-column``, adjusting its value to take the visual indentation
modification in the buffer into account.  When the auto-fill mode is active,
the text wrapping will occur in the same relative position as it would in the
file with the original space-based indentation.

Customization
=============

The ``tbindent`` customization group is used.  It is made a child of the
``Emacs/Editing/Indent`` group. It provides the following customizable user
options:

= ==================================== ================================================
. User-option                          Description
= ==================================== ================================================
. tbindent-lighter                     The minor mode lighter.  Defaults to " ⍈".
. tbindent-target-indent-widths        The target indentation width to use for specified major
                                       modes.  This is an alist of cons ``(mode . width)``
                                       cells.  Some values are defined for some major modes;
                                       you can change the values, remove them and add more.
                                       The **tbindent-mode** will automatically convert
                                       indentation to this specified width for the
                                       corresponding major modes.
. tbindent-target-indent-width-default The default indentation width used by **tbindent-mode**
                                       when nothing is specified for the major mode.
= ==================================== ================================================


Using tbindent mode
===================

**Manual Activation/Deactivation**

Manually toggle the minor mode on/off with ``M-x tbindent-mode``.

The minor mode first checks if the appropriate conditions are met and will
only activate if they are. If not it issues a user error describing the
problem.

**Automatic Activation via major-mode hook**

Add ``tbindent-mode`` in the hook for the major modes were you want it
activated automatically, by doing something like the following (here shown for
C)::

    (add-hook 'c-mode-common-hook #'tbindent-mode)


tbindent Commands
=================

The following commands are provided.

= ==================================== ================================================
. Command                              Description
= ==================================== ================================================
. tbindent-mode                        Toggle the minor mode on/off
. tbindent-set-tab-width               Set the tab and indent width used in current
                                       buffer to N.  Prompts for the width; accepts a
                                       value in the [2, 8] range.

                                       - Set the buffer local value of `tab-width' and
                                         indent control variable(s) used by the current
                                         buffer.
                                       - Use this command to change the indentation width
                                         when **tbindent-mode** is active.

. tbindent-indent-with-tabs            Convert current buffer to use tabs for
                                       indentation. Prompts for the width; accepts a
                                       value in the [2, 8] range.

                                       A utility command, used internally by
                                       **tbindent-mode**. Not available independently
                                       when **tbindent-mode** is active.  Use it when
                                       **tbindent-mode** is turned off to manually
                                       convert the space-based indentation into
                                       tabs-based indentation scheme.


. tbindent-indent-with-spaces          Convert current buffer back to use space-based
                                       indentation.

                                       A utility command, used internally by
                                       **tbindent-mode**.  Not available independently
                                       when **tbindent-mode** is active.  Use it when
                                       **tbindent-mode** is turned off, and after you
                                       used ``tbindent-indent-with-tabs`` to convert
                                       the tabs-based indented buffer back into
                                       space-based indented scheme.
= ==================================== ================================================
