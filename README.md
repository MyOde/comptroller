# comptroller

# WORK IN PROGRESS

Provides compton convenience functions and a wizard mode for making full compton adjustments in realtime with the possibility to commit the newly created configuration.

The motivation for this piece of software is as follows - while running all My windows with a certain amount of global transparency - I often find myself needing to turn down the opacity for a certain window name/class, and maybe even turn it back up. Since I'm too lazy to constantly invoke tools to gather this info and then fumble around with the configuration file - I decided to make a tool that would allow Me to perform this with a button of My keyboard.

The second reason for making this - at one point compton started getting very picky - and some of the option combinations started to create weird artifacts/blinking. To test the various configuration combinations - I wanted to have an interface that would allow Me to fine tune it without manually having to make one adjustment reset - rinse and repeat.

Currently finished interfaces include:
  Convenience function for changing the opacity of an active or specified window (currently only by name)
  Convenience functions for restarting or killing the running compton process

Work in progress
  Wizard mode with two different frontends
    DMenu frontend
    Terminal driven frontend

Fun to have?
  An option to have a pointer appear to select a window the user wants to edit the opacity for.

Works by reading an existing compton configuration file, adding to or changing it - and overwriting the previous configuration.
