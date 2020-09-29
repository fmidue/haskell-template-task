# Haskell Template Task Type for Autotool

## Description

This library is providing a Haskell Template Task Type for the e-learning system
[Autotool](https://gitlab.imn.htwk-leipzig.de/autotool/all0).
This task type enables configuring compiler warnings and errors, and linter
warning and errors, as well as providing feedback for Haskell programming tasks.
The students get a Haskell program template which contains gaps (`undefined`).
In order to successfully complete the task their program has to compile without
error messages, the linter must not throw error messages, the template must be
matched (depending on the configuration different changes might be allowed like
adding parts to the program - beside `undefined`s).

There is a [paper](https://dl.gi.de/handle/20.500.12116/27942) describing the
general approach of this task type as well as
[slides](https://www.uni-due.de/imperia/md/content/fmi/ms-02.10.2019.pdf) of a
talk (both German!).

However, there is no general description on how to configure a specific task.
A good starting point for configuring on tasks would be the default
configuration provided when selecting this task type within Autotool
(Programmierung - Haskell Programmierung - Haskell_Template-Direct).
It features a simple reverse task.
But it does not show the whole power of the approach.
A simple description of the task configuration is:
The configuration is separated into multiple sections.
(Divided by lines of multiple dashes, i.e. `----`)
The first sections is for configuration parameters in YAML format (e.g. for
desired linter advices / errors and compiler warnings / errors).
The second action is the Template which will initially be given to the students.
The following sections contain further modules.
If testing is desired, one of these module needs to be named `Test` which
exports a function named `test`.

## Issues & Pull requests

If you required further explanations on the task type or or having issues with
the task type please file an issue.

Improvements and bug fixes should be shared by filing a pull request.
