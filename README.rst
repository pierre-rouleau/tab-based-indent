==============================================================================
Tab Based Indent -- Edit space-based indented files with tab-based indentation
==============================================================================

🚧 **Under Construction**   🚧


The tab-based-indent (tbindent) package provides a minor mode which provides
the ability to transparently edit a file that is indented with a fixed number
of spaces as if it was indented with hard tabs and therefore control the
visual width of the indentation step by increasing or decreasing the tab
rendering width together with the indentation width.

This minor mode may help people that do not like working with a small
indentation scheme similar to those of Dart or Gleam that imposes a 2-space
indentation scheme format.

Activate the tbindent-mode in those file to seamlessly convert the file to
tab-based indentation scheme using the indentation width that you like to use.
While editing the fill-column is automatically adjusted to take the modified
indentation into account.  When you save the buffer content back to the file,
it is reconverted back to the original space-based indentation format.
