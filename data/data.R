# #Save binding events
# binding_events<-data.frame(
#   Widget=c(rep("GLOBAL",times=11),"tcListbox","tcCombobox"),
#   Event=c("<Button>",
#           "<Button-1>",
#           "<Button-2>",
#           "<Button-3>",
#           "<Motion>",
#           "<ButtonRelease>",
#           "<Double-Button>",
#           "<Enter>",
#           "<Leave>",
#           "<Return>",
#           "<Key>",
#           "<<ListboxSelect>>",
#           "<<ComboboxSelected>>"),
#   Description=c("A mouse button is pressed",
#                 "The left mouse button is pressed",
#                 "The middle mouse button is pressed",
#                 "The right mouse button is pressed",
#                 "A mouse button is pressed and the cursor is moved simultaneously",
#                 "A mouse button is released",
#                 "A mouse button is double clicked",
#                 "The cursor entered a widget",
#                 "The cursor left a widget",
#                 "The Enter key is pressed",
#                 "Any key is pressed",
#                 "A listbox element is selected",
#                 "A combobox element is selected")
# )
#
# common_functions<-data.frame(
#   Syntax=c("tcl('update')",
#            "tk_messageBox()",
#            "tk_select.list()",
#            "tk_choose.files()",
#            "tk_choose.dir()"),
#   Purpose=c("Forces an update of the tcl window.",
#             "Creates a prompt with a message and returns the user's response",
#             "Creates a prompt with a list of options and returns the user's selection.",
#             "Opens a file selection window.",
#             "Opens a directory selection window."),
#   More=c("NA",
#          "?tk_messageBox",
#          "?tk_select.list",
#          "?tk_choose.files",
#          "?tk_choose.dir")
# )
#
# #Define themes
# DEFAULT<-list(
#   Background="gray95",
#   WidgetBackground="gray95",
#   TextColor="black",
#   TextSize=10,
#   TextFamily="Arial"
# )
# LARGE<-list(
#   Background="gray95",
#   WidgetBackground="gray95",
#   TextColor="black",
#   TextSize=15,
#   TextFamily="Arial"
# )
# SMALL<-list(
#   Background="gray95",
#   WidgetBackground="gray95",
#   TextColor="black",
#   TextSize=8,
#   TextFamily="Arial"
# )
# RETRO<-list(
#   Background="violet",
#   WidgetBackground="purple",
#   TextColor="yellow3",
#   TextSize=12,
#   TextFamily="Comic Sans MS"
# )
#
