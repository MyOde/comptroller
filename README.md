# comptroller

# WORK IN PROGRESS

Allows to change values, add new rules for opacity, shading or other compton configuration pieces.

The motivation for this piece of software is as follows - while running all My windows with a certain amount of global transparency - I often find myself needing to turn down the opacity for a certain window name/class, and maybe even turn it back up. Since I'm too lazy to constantly invoke tools to gather this info and then fumble around with the configuration file - I decided to make a tool that would allow Me to perform this with a button of My keyboard.

Planned interfaces include - command line argument based invocations and a dmenu integration.

Works by reading an existing compton configuration file, adding to or changing it - and overwriting the previous configuration.
