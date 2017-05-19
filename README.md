HAB
===

_Make yourself a better person._

(Pending a better name.)

![Master branch build status](https://travis-ci.org/scolej/hab.svg?branch=master)

## What is this?

A real life game for tracking habits. Completely inspired by [Habitica](https://www.habitica.com).

You outline some habits you'd like to enforce or discourage and then
tick them off as you do them.

As you progress you get a status report on your character, for
example:

    Character is level 1 with 2/10 experience and 49 health.
    Experience:    [xxxxxxxx--------------------------------]
    Health:        [xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-]

## How do I specify habits?

Write a file at `~/habit.log` and enter some lines like the following:

    2017-01-01 0000 h mindless-internet-use        -1
    2017-01-01 0000 h work-on-programming-project   1
    2017-01-01 0000 p gym 4d 3

This log creates 3 new habits:

1. `mindless-internet-use` is a negative habit for which you lose a
   single unit of health.
2. `work-on-programming-project` is a positive habit for which you
   gain a point of experience.
3. `gym` is a "periodic habit". If you complete it within 4 days you
   gain 3 experience points, but if you don't complete it within 4
   days you lose 3 points of health.

(The format is a bit rubbish and needs some improvement. The timestamp
lets you add habits and change them as you go along.)

## How do I tick off habits?

Tick them off using the `hab` command:

    $ hab mindless
    1 item(s) checked off
    Lif: -

(You can refer to the habit's full name using just a substring. In
this case, "mindless" will match against "mindless-internet-use")

## How do I install it?

If you have [Stack](https://docs.haskellstack.org/en/stable/README/),
then:

    $ stack install
