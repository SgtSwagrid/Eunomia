<div align="center">

  <h1>🗂️ Eunomia</h1>
  <p>Filtering, ordering and paging of lists for full stack <a href="https://www.scala-lang.org/">Scala</a> websites.</p>

  <span>
    <a href="https://github.com/SgtSwagrid/Eunomia/actions/workflows/build-integrity.yml"><img src="https://github.com/SgtSwagrid/Eunomia/actions/workflows/build-integrity.yml/badge.svg" alt="Build status" /></a>
    <a href="https://search.maven.org/artifact/com.alecdorrington/eunomia-core_3"><img src="https://img.shields.io/maven-central/v/com.alecdorrington/eunomia-core_3.svg" alt="Maven Central" /></a>
    <a href="https://alecdorrington.com/Eunomia"><img src="https://img.shields.io/badge/docs-latest-blue.svg" alt="Documentation" /></a>
  </span>

</div>

> [!WARNING]
> Eunomia is in beta. It is young, it has one user, and anything may change between minor versions.

A library for lists that people filter, order and page, whether the list is a
handful of items or far too many to send to a browser, plus a [Laminar](https://laminar.dev/) table whose header row
takes filters as typed text. It knows nothing of the application it serves.

Named for [Eunomia](https://en.wikipedia.org/wiki/Eunomia), goddess of good order, and daughter of Themis.

## ⬇️ Installation

Add whichever halves you need to your `build.sbt`:

```scala
libraryDependencies += "com.alecdorrington" %% "eunomia-server" % "0.1.0" // On the JVM.
libraryDependencies += "com.alecdorrington" %% "eunomia-client" % "0.1.0" // In the browser.
```

Compiled with Scala `3.8.4`, with no intention to explicitly support older versions.

## 🏯 Layout

| Module | Platform | Contents |
|--------|----------|----------|
| [`eunomia-core`](core)     | JVM + JS | The query model, the shared list schema, and the endpoint inputs. |
| [`eunomia-server`](server) | JVM      | Answering queries over a database table, with any Slick profile.  |
| [`eunomia-client`](client) | JS       | Headless list state, and an unstyled table component.             |

## Defining a list

Describe the fields of a list once, in code shared by server and client:

```scala
import com.alecdorrington.eunomia.model.*

object BookList:
  val name   = Field.of[Book]("name", _.name)     // Field[Book, String]
  val rating = Field.of[Book]("rating", _.rating) // Field[Book, Long], from an Option[Long]
  val schema = Schema(name, rating)
```

## Serving it

Map each field to the column storing it. The mapping is checked against the schema when the
server starts, so a field cannot be forgotten. Restrict the rows to what the user may see, and
hand over the query:

```scala
import com.alecdorrington.eunomia.api.ListApi
import com.alecdorrington.eunomia.server.SqlLists

val lists   = SqlLists(H2Profile)
val columns = lists.columns(BookList.schema)(
  lists.text[Books]("name")(_.name.?),
  lists.whole[Books]("rating")(_.rating),
)

val listBooks = endpoint.get.in("api" / "books").in(ListApi.input).out(ListApi.reply[Book])

listBooks.serverLogic(query =>
  lists.answer(books.filter(_.owner === user).sortBy(_.id), columns, query)
    .fold(problem => IO.pure(Left(problem)), action => db.run(action).map(Right(_))),
)
```

## Showing it

```scala
import com.alecdorrington.eunomia.client.*

val view = ListView(BookList.schema, ListSource.endpoint[Book]("/api/books"))
// Or, for items the application already holds in a signal: ListSource.items(books)

Table(
  view,
  List(Table.Column.of("Name", BookList.name), Table.Column.of("Rating", BookList.rating)),
  key = _.id,
)
```

## Where a query runs

Nowhere in the application, deliberately: the library decides, per request.

The server, which alone knows how many rows a user may see, answers a short list
(`wholeUpTo`, 200 rows by default) by sending it whole. The client keeps it and runs every later
query itself, so typing into a header cell filters instantly, with no further requests. A long
list is filtered, ordered and paged in SQL instead, and only the window asked for is sent
(at most `maxWindow` rows, 100 by default, whatever the request says). The client then sends each
query once it has stopped changing, and discards replies to queries since superseded.

Both paths give identical results, as the semantics are SQL's throughout: a comparison with an
absent value never holds (`!(rating >= 50)` does include unrated books), absent values sort last
in either direction, and text is sought ignoring case. The test suites check this agreement.

The rules of thumb this encodes, for anyone tuning the thresholds:

- **Short and bounded lists** (one user's own items, one project's labels) are cheapest sent once:
  queries then cost nothing and answer instantly.
- **Lists that grow with other people's activity** (every comment on a post, an audit log)
  must be paged in the database, or bandwidth, memory and time to first paint grow without limit.
- **Heavy items** (documents with their full text) should be listed as light summary rows, with the
  full item fetched on selection, whichever path the list takes.
- **Access control always stays on the server**, in the base query handed to `answer`: the whole
  list sent to a browser is only ever the rows its user may see.
- **Ordering reveals what it orders by**, even with the values hidden: a list sorted by a withheld
  field leaks its ranking. A list can only be filtered and ordered by the fields of its schema,
  and `columns` must store exactly those, so give a list whose readers may not see a field its
  own schema without that field, rather than hiding the column's values.
- **Indexes** matter once lists are long: ordering or filtering by an unindexed column is a full scan.

## Typing a filter

A person filters one column at a time, typing into its header cell. `CellFilter` reads that text
against the column's kind: `novel`, `!draft`, `=Alpha` for text; `>=50 <80`, `50..80`, `!=0` for
numbers; `yes`/`no` for truth values; `?` or `!?` for absence and presence; `|` between alternatives.
Queries built in code compose the same way: `(rating >= 50L && !name.contains("draft")) || rating.missing`.

## 🤝 Contributing

Eunomia is developed as part of a larger private project, of which this repository is an automatically synchronised
mirror (by [GitHub Graph](https://github.com/SgtSwagrid/github-graph)), so changes made here directly would be overwritten.
Issues are very welcome; for anything more, please open an issue first.

## 👁️ See also

- [Hecate](https://github.com/SgtSwagrid/Hecate), a sibling, for user accounts, groups and permissions.
- [Iris](https://github.com/SgtSwagrid/Iris), a sibling, a provider-agnostic client for large language models.
- This library was made using [Scala Library Template](https://github.com/SgtSwagrid/scala-library-template).
