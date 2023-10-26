#lang scribble/manual

@(require racket/string
          scribble/core
          scribble/html-properties
          "shortcut.rkt")

@(define-syntax-rule (kbd sym ...)
  (let ([text (shortcut 'sym ...)])
    (elemref `("kbd" ,text) text)))

@(define-syntax-rule (defkbd (sym ...) pre-content ...)
  (let ([text (shortcut 'sym ...)]
        [title (shortcut-alt 'sym ...)])
    (elem
      (elemtag `("kbd" ,text))
      (elem text #:style (make-style "kbd" (list (make-attributes `((title . ,title))))))
      " --- "
      pre-content ...)))

@title{Franz: Mac and Windows Client for Apache Kafka}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@(define (homepage text)
  (link "https://franz.defn.io" text))

@(define (kafka . text)
  (apply link "https://kafka.apache.org/" text))

@homepage{Franz} is a native Mac and Windows client for @kafka{Apache
Kafka}. It helps you manage your Kafka clusters, topics and consumer
groups and it provides convenient functionality for monitoring the data
being published to topics.


@section{Connections}

When you start Franz, you are presented with the @deftech{Welcome
Window}. From the Welcome Window you can connect to servers you've
previously used or create new connections.

On macOS, you can access the @tech{Welcome Window} using the ``Window''
@tt{->} ``Welcome to Franz'' menu item or by pressing @kbd[shift cmd
1]. On Windows, the window is accessible via the the ``Help'' @tt{->}
``Welcome to Franz'' menu item or by pressing @kbd[shift ctl 1].

@subsection{Security}

Connection metadata is stored inside Franz' internal metadata database,
but passwords are stored in the macOS Keychain. On Windows, passwords
are encrypted using the Windows @tt{CryptProtectData} API, meaning
passwords can only be decrypted by the user account they were originally
encrypted by.

@subsection{Workspaces}

When you connect to a Kafka cluster, a @deftech{Workspace Window} is
opened for that cluster. All operations within the workspace operate
on the same connection. When you close a Workspace Window, all of its
associated connections and interface objects are closed.

@subsection{Topics}

From the @tech{Workspace Window} sidebar, you can select topics to
view general information about them, to browse through their record
data and publish new data, or to view their configuration.

You can publish new data on a topic by pressing the plus icon on the
top right corner of the @tech{Workspace Window} toolbar.

@subsubsection{Information Tab}

The @deftech{Information} tab (@kbd[cmd 1]) displays general
information about the selected topic.

@subsubsection{Records Table Tab}

The @deftech{Records Table} tab (@kbd[cmd 2]) on a topic lets you
stream live data being published on a topic or jump to any offset you
like and paginate through the data manually.

When you open the Records Table tab on a topic, it immediately starts
streaming recent data into the table.  You can stop this by pressing
the ``Toggle Live Mode'' button on the bottom left corner of the
table.  You can configure how much data is requested from the topic on
each fetch by click the ``Options...'' button in the bottom right, and
you can manually load more data by pressing the ``Load More
Records...''  button.

From the ``Options...'' popover, you can also jump to any offset you
like.  See @tech{Jump Popover} for details.

You can right-click on any record with a non-null key to publish a
tombstone for it.  Additionally, you can drag and drop any non-null
key or value from the table to any application that accepts files to
export the dragged value.  You can use the ``Key Format'' and ``Value
Format'' options from the ``Options...'' popover to control what
format the columns are exported as.

Double-clicking any record will bring up its @tech{Record Detail
Window}.

@subsubsection{Records Table Scripting}

@(define (lua-anchor text)
  (link "https://lua.org" text))

You can control the values displayed in the @tech{Records Table} by
writing @lua-anchor{Lua} scripts.  With the @tech{Records Table} for a
topic selected, press the scripting button -- located in the center
bottom of the table -- to bring up the @deftech{scripting window}.
Using the scripting window, you can edit the @tt{transform} function
to control how data is presented in the @tech{Records Table}.

To activate and deactivate a script, press the bolt icon in the
@tech{scripting window} toolbar or use the @kbd[cmd return] keyboard
shortcut.  After a script is activated, any changes made to the text
of the script will cause it to be deactivated.

The @tt{record} argument to the @tt{transform} function is a Lua table
with the following fields:

@tabular[
#:style 'boxed
#:column-properties '(left right)
#:row-properties '(bottom-border ())
(list
 (list @bold{field}      @bold{description})
 (list @tt{partition_id} @elem{the partition the record was published to})
 (list @tt{offset}       @elem{the record's offset as a non-negative integer})
 (list @tt{timestamp}    @elem{the record's timestamp in milliseconds since the UNIX epoch})
 (list @tt{key}          @elem{the record's key as a string or @tt{nil}})
 (list @tt{value}        @elem{the record's value as a string or @tt{nil}}))]

You may modify any of these fields to control how the record is
displayed in the @tech{Records Table}.  Changing the data types of
these fields is prohibited and will lead to an error when data gets
loaded.

Returning @tt{nil} from the @tt{transform} function will cause the
record to be skipped in the @tech{Records Table}.  You can leverage
this to, for example, filter records by partition:

@codeblock[#:keep-lang-line? #f]|{
  #lang lua
  function script.transform(record)
    if record.partition_id ~= 2 then
      return nil
    end
    return record
  end
}|

Within the scripting environment, a @tt{json} table is provided with
functions for encoding and decoding JSON data.  For example, the
following script can be used to read the @tt{example} property of the
record's JSON value:

@codeblock[#:keep-lang-line? #f]|{
  #lang lua
  local script = {}

  function script.transform(record)
    record.value = json.decode(record.value).example
    return record
  end

  return script
}|

See the @secref{ref} for a list of all the functionality available
within the scripting environment.

@subsubsection{Jumping to Offsets}

From the ``Options...'' popover of a @tech{Records Table}, push the
``Jump...'' button to get to the @deftech{Jump Popover} (@kbd[shift
cmd J]).  From there, you can reset the record iterator to various
offsets, as described below.

@subsubsub*section{Earliest}

Queries each partition for its earliest offset and moves the iterator
back.  This is slightly different than explicitly resetting all
partitions to offset 0 as the first offset on a partition might not
necessarily be 0 (as in the case of compacted records).  Functionally,
however, it has the same effect: the iterator will start iterating
through records from the very beginning of the topic's history.

@subsubsub*section{Timestamp}

Queries each partition for the first offset on or after the given date
and time and moves the iterator there.  For partitions where the
timestamp represents a time after the latest offset, it makes an
additional query to find the latest offset.

@subsubsub*section{Recent}

Queries each partition for its latest offset and moves the iterator to
that position, minus the requested delta.

@subsubsub*section{Latest}

Queries each partition for its latest offset and moves the iterator
forward.

@subsubsub*section{Offset}

Moves the iterator to the given offset for every partition.  For
partitions that are behind the selected offset, no new data will be
received until they reach it.

@subsubsection{Consumer Groups Tab}

The @deftech{Consumer Groups Tab} (@kbd[cmd 3]) displays the active
consumer groups for the selected topic. This is an easy way to discover
what groups are actively reading from individual topics.

@subsubsection{Configuration Table Tab}

The @deftech{Configuration Table} (@kbd[cmd 4]) tab displays the
selected topic's configuration. Non-default values are presented in bold
and sensitive values are hidden by default. You may reveal sensitive
values by right clicking on them and pressing the ``Reveal'' context
menu item.

Certain configuration options may be editable. The context menu for
those entries will have ``Edit...'' and ``Delete'' menu items on their
context menus. Edits are made in batches and have to be manually applied
by pushing the ``Apply'' button at the bottom of the table. Edits can
be reset by pressing the ``Reset'' button. Switching tabs after making
edits will discard all unapplied changes.

@subsection{Record Detail Window}

The @deftech{Record Detail Window} displays the contents of individual
records.  You can configure the default format for the key and the
value on a per-topic basis by customizing the ``Key Format'' and the
``Value Format'' from the @tech{Records Table} ``Options...'' popover.

@subsection{Consumer Groups}

When you select a consumer group from the @tech{Workspace Window}
sidebar, you are presented with the @deftech{Consumer Offsets Table}.
There, you can see member assignments, offsets and lag as well as
reset individual offsets by right-clicking any of the entries.

You may only reset offsets if the consumer group is in the empty
state.

@subsection{Schema Registry}

With a @tech{Workspace Window} in the foreground, you can configure a
@deftech{Schema Registry} from the main menu by selecting ``Schema
Registry'' @tt{->} ``Configure...''.  Once a registry is configured,
records are automatically converted to JSON according to the schemas
found in the registry before being displayed in the @tech{Records
Table} and before being passed to any Lua scripts.  To remove a
registry, open the configuration window and remove its URL then press
``Save''.

@(define (registry-yt . pre-content)
   (apply link "https://www.youtube.com/watch?v=RESS4IdghxU" pre-content))

See @registry-yt{this YouTube video} for a live demo.


@section{Keyboard Shortcuts (macOS)}

@defkbd[(shift cmd 1)]{
  Displays the @tech{Welcome Window}.
}

@defkbd[(shift cmd J)]{
  With a topic @tech{Records Table} visible, turns off live mode (if
  on) and displays the @tech{Jump Popover}.
}

@defkbd[(cmd 1)]{
  With a broker or topic selected, switches to the @tech{Information}
  tab.
}

@defkbd[(cmd 2)]{
  With a topic selected, switches to the @tech{Records Table} tab.
  With a broker or consumer group selected, switches to the
  Configuration tab.
}

@defkbd[(cmd 3)]{
  With a topic selected, switches to the @tech{Consumer Groups Tab}.
}

@defkbd[(cmd 4)]{
  With a topic selected, switches to the Configuration tab.
}

@defkbd[(cmd R)]{
  Within a @tech{Workspace Window}, reloads the connection metadata.
}

@defkbd[(cmd T)]{
  Within a @tech{Workspace Window}, duplicates the Workspace in a new
  tab.
}

@defkbd[(shift cmd T)]{
  Within a @tech{Workspace Window}, duplicates the Workspace in a new
  window.
}

@defkbd[(cmd shift return)]{
  Within a @tech{Scripting Window}, runs the script against the current
  batch of loaded records.
}

@defkbd[(cmd return)]{
  Within a @tech{Scripting Window}, activates or deactivates the script.
}

@defkbd[(cmd @",")]{
  Opens the Preferences Window.
}


@section{Keyboard Shortcuts (Linux & Windows)}

@defkbd[(shift ctl 1)]{
  Displays the @tech{Welcome Window}.
}

@defkbd[(shift ctl N)]{
  Within a @tech{Workspace Window}, duplicates the Workspace in a new
  window.
}

@defkbd[(ctl R)]{
  Within a @tech{Workspace Window}, reloads the connection metadata.
}

@defkbd[(ctl shift return)]{
  Within a @tech{Scripting Window}, runs the script against the current
  batch of loaded records.
}

@defkbd[(ctl return)]{
  Within a @tech{Scripting Window}, activates or deactivates the script.
}

@defkbd[(ctl @";")]{
  Within a @tech{Workspace Window}, opens the Preferences Window.
}


@section{Known Issues and Limitations}

If any of these limitations are showstoppers for you, please e-mail me
at @tt{bogdan@"@"defn.io} and let me know.

@subsection[#:tag "limitations-schema-registry"]{Schema Registry}

The only type of @tech{schema registry} currently supported is the
Confluent Schema Registry.


@section[#:tag "ref"]{Scripting Reference}

@(define-syntax-rule (deflua id (arg ...) res pre-content ...)
   (let ([id-str (symbol->string 'id)]
         [args-str (string-join (map symbol->string '(arg ...)) ", ")])
     (tabular
      #:style 'boxed
      #:column-properties '(left right)
      #:row-properties '(bottom-border ())
      (list
       (list @elem[(elemtag `("lua" ,id-str)) @tt[@|id-str| "(" @args-str ")"]] @tt[@symbol->string['res]])
       (list @nested[pre-content ...] 'cont)))))

@(define-syntax-rule (lua id)
  (let ([id-str (symbol->string 'id)])
    (elemref `("lua" ,id-str) id-str)))

@deflua[avro.parse (str) avro.Codec]{
  Decodes the Apache Avro schema represented by @tt{str} and returns a
  @tt{avro.Codec}.  Raises an error if the schema is invalid.

  Use the @lua[avro.Codec:read] method on the returned codec to decode
  data.
}

@deflua[avro.Codec:read (str) any]{
  Reads the data in @tt{str} according to its internal schema.  Avro
  records are represented by Lua tables with keys named after every
  field.  Arrays are represented by integer-indexed tables.  Enums are
  represented by strings.  Unions are represented by tables with
  exactly two keys: a @tt{type} key referencing the fully-qualified
  type name of the value and a @tt{value} key containing the value.
  Bytes and string values both map to Lua strings.  All other
  primitive values map to Lua values in the way you would expect.

  See @secref["decoding-avro-data"] for an example.
}

@deflua[json.decode (str) table]{
  Decodes the JSON data in @tt{str} to a Lua table.
}

@deflua[json.encode (v) string]{
  Encodes the Lua value @tt{v} to JSON.
}

@deflua[kafka.parse_committed_offset (record) tuple]{
  Decodes committed offset data off of the @tt{__committed_offsets}
  topic.  On failure, returns @tt{nil}.  On success, returns a string
  representing the type of event that was decoded and a value
  representing that event.  The currently-supported event types are
  @racket["offset_commit"] and @racket["group_metadata"].  For
  example:

  @codeblock[#:keep-lang-line? #f]|{
    #lang lua
    local script = {}

    function script.transform(record)
      local event_type, data = kafka.parse_committed_offset(record)
      if event_type == nil then
        return record
      end
      record.value = tostring(data) -- or json.encode(data)
      return record
    end

    return script
  }|
}

@deflua[kafka.record_size (record) number]{
  Returns the size in bytes of the given record.
}

@deflua[math.abs (n) number]{
  Returns the absolute value of @tt{n}.
}

@deflua[math.acos (n) number]{
  Returns the arc cosine of @tt{n} in radians.
}

@deflua[math.asin (n) number]{
  Returns the arc sine of @tt{n} in radians.
}

@deflua[math.atan (y x) number]{
  Returns the arc tangent of @tt{y/x}.  The @tt{x} argument defaults
  to @racket[1] if not provided.
}

@deflua[math.ceil (n) number]{
  Rounds @tt{n} towards positive infinity.
}

@deflua[math.cos (n) number]{
  Returns the cosine of @tt{n}.
}

@deflua[math.deg (n) number]{
  Converts the angle @tt{n} from radians to degrees.
}

@deflua[math.exp (n) number]{
  Raises the base of natural logarithms to @tt{n} (@tt{e^n}).
}

@deflua[math.floor (n) number]{
  Rounds @tt{n} towards negative infinity.
}

@deflua[math.log (n base) number]{
  Returns the logarithm of @tt{x} in the given @tt{base}.  The
  @tt{base} argument defaults to @tt{e}.
}

@deflua[math.max (n ...) number]{
  Returns the largest number amongst the given set.
}

@deflua[math.min (n ...) number]{
  Returns the smallest number amongst the given set.
}

@deflua[math.rad (n) number]{
  Converts the angle @tt{n} from degrees to radians.
}

@deflua[math.random (m n) number]{
  With no arguments, returns a random number in the range @tt{[0.0,
  1.0]}.  With one argument, returns a random number in the range
  @tt{[0.0, m]}.  With two arguments, returns a random number in the
  range @tt{[m, n]}.
}

@deflua[math.randomseed (x y) number]{
  With no arguments, seeds the random number generator arbitrarily.
  With one argument, seeds the random number generator to @tt{x}.
  With two arguments, seeds the random number generator to
  @tt{x ~ y & 0x7FFFFFFF}.
}

@deflua[math.sin (n) number]{
  Returns the sine of @tt{n}.
}

@deflua[math.sqrt (n) number]{
  Returns the square root of @tt{n}.
}

@deflua[math.tan (n) number]{
  Returns the tangent of @tt{n}.
}

@deflua[math.tointeger (v) number]{
  Converts @tt{v} to an integer.  Returns @tt{false} if the value
  cannot be converted.
}

@deflua[msgpack.unpack (str) any]{
  Decodes the MessagePack-encoded value represented by @tt{str} to
  Lua.  Arrays and maps are represented as Lua tables.  Strings and
  binary data are represented as Lua strings.  Nil is represented as
  Lua @tt{nil}.

  See @secref["decoding-msgpack-data"] for an example.
}

@deflua[string.byte (str i j) table]{
  Returns the bytes in @tt{str} between @tt{i} and @tt{j}.  The @tt{i}
  argument defaults to @tt{1} and the @tt{j} argument defaults to the
  length of @tt{str}.
}

@deflua[string.char (...) string]{
  Constructs a string from the given bytes.
}

@deflua[string.format (fmt ...) string]{
  Formats the variable arguments according to @tt{fmt}.  Behaves the
  same as the standard C function @tt{sprintf}, except that there is
  an additional conversion specifier, @tt{%q}, which quotes literal
  Lua values.
}

@deflua[string.len (str) number]{
  Returns the length of @tt{str}.
}

@deflua[string.lower (str) string]{
  Returns a new string with the characters in @tt{str} lowercased
  according to the current locale.
}

@deflua[string.rep (str n sep) string]{
  Repeats @tt{str} @tt{n} times, interspersing @tt{sep} between
  repetitions.  The @tt{sep} argument defaults to @racket[""].
}

@deflua[string.reverse (str) string]{
  Returns a new string with the characters in @tt{str} in reverse
  order.
}

@deflua[string.sub (str i j) string]{
  Returns a substring of @tt{str} starting from @tt{i} until @tt{j}.
  The @tt{i} argument defaults to @racket[1] and the @tt{j} argument
  defaults to the length of @tt{str}.
}

@deflua[string.upper (str) string]{
  Returns a new string with the characters in @tt{str} uppercased
  according to the current locale.
}

@subsection{Examples}

@subsubsection[#:tag "decoding-json-data"]{Decoding JSON Data}

Use @lua[json.decode] to decode your data.

@codeblock[#:keep-lang-line? #f]|{
  #lang lua
  local script = {}

  function script.transform(record)
    local object = json.decode(record.value)
    record.value = tostring(object.field)
    return record
  end

  return script
}|

@subsubsection[#:tag "decoding-avro-data"]{Decoding Avro Data}

@(define (avro-yt . pre-content)
   (apply link "https://www.youtube.com/watch?v=suUseaJ3bJI" pre-content))

Use @lua[avro.parse] to convert an Avro Schema into a codec.  Then,
use that codec to decode your record data.

@codeblock[#:keep-lang-line? #f]|{
  #lang lua
  local script = {}
  local schema = [[
    {
      "type": "record",
      "name": "Person",
      "fields": [
        {
          "name": "Name",
          "type": "string"
        },
        {
          "name": "Age",
          "type": "int"
        }
      ]
    }
  ]]
  local person_codec = avro.parse(schema)

  function script.transform(record)
    local person = person_codec:read(record.value)
    record.value = person.Name
    return record
  end

  return script
}|

You can write your schema as a Lua table and convert it to JSON using
@lua[json.encode].  For example, you could rewrite the above example
to:

@codeblock[#:keep-lang-line? #f]|{
  #lang lua
  local script = {}
  local schema = json.encode(
    {
      type = "record",
      name = "Person",
      fields = {
        { name = "Name", type = "string" },
        { name = "Age",  type = "int" }
      }
    }
  )
  local person_codec = avro.parse(schema)

  function script.transform(record)
    local person = person_codec:read(record.value)
    record.value = person.Name
    return record
  end

  return script
}|

See @avro-yt{this YouTube video} for a live demo.

@subsubsection[#:tag "decoding-msgpack-data"]{Decoding MessagePack Data}

Use @lua[msgpack.unpack] to decode your data.

@codeblock[#:keep-lang-line? #f]|{
  #lang lua
  local script = {}

  function script.transform(record)
    local object = msgpack.unpack(record.value)
    record.value = tostring(object.field)
    return record
  end

  return script
}|


@section{Guides}

@subsection{Mutual TLS}

Franz supports connecting to Kafka servers with mTLS (also known as
``two-way SSL'') enabled. To connect to a server with mTLS, merely
provide an SSL Key and an SSL Certificate during connection setup.

@subsubsection{How to extract Java Keystore keys & certificates}

If your client key and certificates are stored in a Java Keystore file
(typically, a file with the @tt{.jks} extension), then you must first
extract and convert them to PEM format. You can do this using the
@tt{keytool} utility provided by your Java Runtime Environment. For
example, assuming you have a keystore file named @filepath{client.jks},
you can run the following command:

@verbatim{
  keytool \
    -importkeystore \
    -srckeystore client.jks \
    -destkeystore client.p12 \
    -srcstoretype jks \
    -deststoretype pkcs12
}

The result is a PKCS12 store named @filepath{client.p12}. Next, convert
this store to PEM format by running:

@verbatim{
  openssl pkcs12 \
    -nodes \
    -in client.p12 \
    -out client.pem
}

Finally, select the @filepath{client.pem} file as @emph{both} the SSL
Key and the SSL Cert from the Franz Connection Dialog and connect to
your broker.

@section{Privacy}

Apart from when checking for updates, Franz never phones home for any
reason.  Automatic Updates can be turned off from the Preferences
Window (@kbd[cmd @","]).

@section{Credits}

@(define (racket-anchor . text)
  (apply link "https://racket-lang.org/" text))

Franz is built using the @racket-anchor{Racket programming language}
and distributes its runtime alongside the application.  Racket is
licensed under the MIT License.

The source code for Franz is available for all to read on
@link["https://github.com/Bogdanp/Franz"]{GitHub}.
