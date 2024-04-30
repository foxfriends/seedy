:- use_module('postgresql/postgresql').
:- use_module('postgresql/sql_query').
:- use_module(library(os)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(charsio)).

main :-
    argv([File]),
    atom_chars(F, File),
    use_module(F),
    assertz((existing(_) :- false)),
    generate_migration,
    halt.

generate_migration :-
    connection(C),
    forall(table(Name, Opts), load_existing(C, Name, Opts)),
    forall(table(Name, Opts), delete_removed(Name, Opts)),
    forall(table(Name, Opts), insert_created(Name, Opts)).

load_existing(C, Name, Opts) :-
    findall(X, (member(column(X, T), Opts), \+member(generated, T)), Names),
    Select =.. [select | Names],
    sql(C, [Select, from(Name)], data(Rows)),
    forall((member(Row, Rows), Entry =.. [Name | Row]), assertz(existing(Entry))).

delete_removed(Name, Opts) :-
    findall(_, (member(column(_, T), Opts), \+member(generated, T)), Vals),
    forall(
        (Call =.. [Name | Vals], existing(Call), \+call(Call)),
        write_delete(Name, Opts, Call)
    ).

insert_created(Name, Opts) :-
    forall(
        (
            findall(_, (member(column(_, T), Opts), \+member(generated, T)), Vals),
            Call =.. [Name | Vals],
            call(Call)
        ),
        write_insert(Name, Opts, Call)
    ).

atomic_join([A], _, A).
atomic_join([A | As], J, S) :-
    atomic_join(As, J, S1),
    atom_concat(A, J, S0),
    atom_concat(S0, S1, S).

escape(sql(null), 'NULL') :- !.
escape(array(C), E) :-
    maplist(escape, C, Escaped),
    atomic_join(Escaped, ',', B),
    atom_concat('{', B, E0),
    atom_concat(E0, '}', E).

escape(C, C) :- integer(C), !.
escape(C, C) :- float(C), !.
escape(C, E) :-
    atom(C),
    atom_chars(C, Chars),
    escape_str(Chars, Escaped),
    quote(Escaped, Quoted),
    atom_chars(E, Quoted).
escape(C, E) :-
    escape_str(C, Escaped),
    quote(Escaped, Quoted),
    atom_chars(E, Quoted).

escape_str("", "").
escape_str(['\'' | Cs], ['\'', '\'' | Es]) :- !, escape_str(Cs, Es).
escape_str([C | Cs], [C | Es]) :- escape_str(Cs, Es).

quote(S, Q) :- append(["'", S, "'"], Q).

cols_generated(Opts, Generated) :-
    findall(N, (member(column(N, T), Opts), member(generated, T)), Generated).
cols_key(Opts, Keys) :-
    findall(N, (member(column(N, T), Opts), member(key, T)), Keys).
cols_free(Opts, Free) :-
    findall(N, (member(column(N, T), Opts), \+member(generated, T)), Free).
cols_value(Opts, Keys) :-
    findall(N, (member(column(N, T), Opts), \+member(generated, T), \+member(key, T)), Keys).

write_insert(Name, Opts, Call) :-
    Call =.. [Name | Vals],
    maplist(escape, Vals, Escaped),
    Values =.. [values | Escaped],
    cols_free(Opts, Cols),
    cols_key(Opts, Keys),
    cols_value(Opts, ToUpdate),
    on_conflict_update(Keys, ToUpdate, Conflict),
    sql_query([insert_into(Name, Cols), Values | Conflict], Sql, []),
    format("~s;~n", [Sql]).

on_conflict_update([], _, []) :- !.
on_conflict_update(_, [], []) :- !.
on_conflict_update(Keys, Cols, [OnConflict, Set]) :-
    OnConflict =.. [on_conflict_do_update | Keys],
    maplist(set_old_new, Cols, Conds),
    set(Conds, Set).

set_old_new(Col, Col=New) :-
    atom_concat('EXCLUDED.', Col, New).

pair(A, B, A-B).

where([Term], where(Term)).
where([Term, T2 | Terms], where((Term, Where))) :- where([T2 | Terms], where(Where)).

set([Term], set(Term)).
set([Term, T2 | Terms], set((Term, Set))) :- set([T2 | Terms], set(Set)).

write_delete(Name, Opts, Call) :-
    Call =.. [Name | Vals],
    findall(C, (member(C, Opts), C = column(N, T), \+member(generated, T)), Cols),
    maplist(pair, Vals, Cols, Kvs),
    findall(N=E, (member(V-column(N, T), Kvs), member(key, T), escape(V, E)), Cond),
    where(Cond, Where),
    sql_query([delete(Name), Where], Sql, []),
    format("~s;~n", [Sql]).

:- initialization(main).
