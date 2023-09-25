# Seedy: Declarative seed manager

> Very concept phase, not even built. Does it sound useful?

Declare your seed data as data in code, version it as you would any other code.

Then use `seedy` to keep that data up to date in your running databases. Similar to how
many migration-generating tools generate migrations based on your current database state,
`seedy` can update the state of managed seed data in those database.

Unlike migrations, historical seeds are not required. Instead `seedy` just works off
the current data in your database and brings it forwards. A little bit of extra metadata
is stored in an extra table (similar to the table migration runners store in your database)
to maintain knowledge of the managed data. No concessions need to be made to your actual
data structures in order for this to work. In this sense, `seedy` actually works more like
tools like Terraform, that maintain state over a transient source of truth.
