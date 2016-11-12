ANNALES
=======

An Empire, beset by warring tribes. Structure:

* Reign (of an Emperor)
* Incursions and battles by tribes (analogous to peripheral characters)
* Intrigues (against the Emperor by his kin, which are generated)
* Disasters
* Projects 
* Death of the Emperor (either as an outcome of intrigue, in a battle, or by magical or natural causes)

## Basic generators

* names of people
* names of tribes
* attributes of tribes "warlike" "peaceful" "savage"
* activities of tribes "given to X"
* animals - worshipped, hunted, etc
* plants - eaten, worshipped
* places (forests, marshes, rivers, coastlands)
* A set of military generators
* ranks and offices
* relations (niece, son-in-law, etc)
* causes of death
* illnesses, curses and maladies
* disasters
* things to build in projects

## A reign

A reign starts with a court (people) and a set of currently active
tribes.

During the reign, the court can conduct intrigues, which may lead to
the death of the emperor or empress

The tribes war with one another, possibly leaving the history

Tribes leave by being enslaved, defeated, massacred, migrating away,
curses

New tribes may arise

Tribes form alliances

The ruler may build something as part of a project

The ruler may fall victim to a curse, illness or madness

New members of the court are created - royal births, ambitious
generals and priests, etc

New religions are promulgated

Artefacts found from extinct tribes

Ablative clauses are good (the Illuminati being subdued, the Emperor
embarked on a project to repair the quays)


## Technical notes

Possible, more interestingly Haskell-ish way to do this: wrap the
TextGen state in a second state monad which carries the current
narrative state around, something like

    narstate = {
        emperor: ( "Charles", 2 ),
        court: [ "Umberto", "Godfrey", "Peter the Blunt" ],
        tribes: [ "Unicorns", "Vacuums", "Haversathes" ]
        }

    do
        state <- return initState
        ( text, state2 ) <- doIncident state
