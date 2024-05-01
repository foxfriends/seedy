# Seedy

Database seed data management using Prolog.

The intended use case of this tool is to allow the developer to describe their database seed data
declaratively, and then generate a SQL script that will transform the database's current state to
what is being described.

Inspired by the constant headache of trying to create and maintain any "required data entries"
of production quality seed data, this tool makes that possible and easy, using the already
declarative and relational nature of Prolog.

## Usage

__Step 1__: Write your seed file as a Prolog module. This module must export specific predicates:

* `connection(-Connection)`: Creates a connection to the database using the `postgresql-prolog` library
* `table(?Name, ?Columns)`: Defines all the tables that are to be managed

Then, for each table described by the seed's implementation of `table/2`, a predicate with the
same name and number of arguments as described by the table declaration.

Further details will be described later, when this tool is actually working well.

```prolog
:- module(seed, [connection/1, table/2, default_users/3, default_rooms/2, user_rooms/2]).
:- use_module(library(format), [format/2]).

connection(C) :- postgresql:connect("seedy", "seedy", '127.0.0.1', 5432, "seedy", C).

table(default_users, [
    column(id, [type(string), generated, pk]),
    column(name, [type(string), key]),
    column(display_name, [type(string)]),
]).

table(default_rooms, [
    column(id, [type(string), generated, pk]),
    column(name, [type(string), key]),
    column(description, [type(string)])
]).

table(user_rooms, [
    column(id, [type(string), generated, pk]),
    column(user_id, [type(reference(default_users)), key]),
    column(room_id, [type(reference(default_rooms)), key])
]).

default_users("foxfriends", "Cam").
default_users("mthom", "Mark").
default_users("aarroyoc", "Adrián").

default_rooms("general", "Regular stuff").
default_rooms("random", "Random stuff").
default_rooms("prolog", "Prolog stuff").

user_rooms(default_users(I), default_rooms("general")) :- default_users(I, _, _).
user_rooms(default_users("aarroyoc"), default_rooms("prolog")).
user_rooms(default_users("mthom"), default_rooms("prolog")).
user_rooms(default_users("aarroyoc"), default_rooms("random")).
user_rooms(default_users("foxfriends"), default_rooms("random")).
```

__Step 2__: Run `scryer-prolog seeder.pl -- seed.pl` to generate a SQL script (on STDOUT) which can be run however you choose to run it.

## Implementation Details

Due to various limitations:
* This only supports Postgres, and specifically only Postgres as supported by the underlying [postgresql-prolog][] library.
* This only supports [Scryer Prolog][]

[postgresql-prolog]: https://github.com/aarroyoc/postgresql-prolog
[Scryer Prolog]: https://github.com/mthom/scryer-prolog

