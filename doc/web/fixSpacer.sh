find . -iname "*.htm" -exec sed "s|class=\"spacer\" .*width=\"100%\"> *<br>|class=\"spacer\" width=\"100%\">|g" '{}' --in-place  \;
