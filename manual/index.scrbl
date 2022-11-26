#lang scribble/manual

@(require racket/string
          "shortcut.rkt")

@(define-syntax-rule (kbd sym ...)
  (let ([text (shortcut 'sym ...)])
    (elemref `("kbd" ,text) text)))

@(define-syntax-rule (defkbd (sym ...) pre-content ...)
  (let ([text (shortcut 'sym ...)])
    (elem
      (elemtag `("kbd" ,text))
      text
      " --- "
      pre-content ...)))

@title{Franz: macOS Client for Apache Kafka}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@(define (homepage text)
  (link "https://franz.defn.io" text))

@(define (kafka . text)
  (apply link "https://kafka.apache.org/" text))

@homepage{Franz} is a native macOS client for @kafka{Apache Kafka}.
It helps you manage your Kafka clusters, topics and consumer groups
and it provides convenient functionality for monitoring the data being
published to topics.

Franz is currently in beta and free during the beta period.  Beta
builds expire after 90 days, so be sure to update when new builds come
out.  Once out of Beta, you will be able to purchase a perpetual
license for a small fee.

@section{Connections}

When you start Franz, you are presented with the @deftech{Welcome
Screen}.  From the Welcome Screen you can connect to servers you've
previously used or create new connections.

You can access the @tech{Welcome Screen} using the ``Window'' @tt{->}
``Welcome to Franz'' menu item or by pressing @kbd[shift cmd 1].

@subsection{Security}

Connection metadata is stored inside Franz' internal metadata
database, but passwords are stored in the macOS Keychain.

@subsection{Workspaces}

When you connect to a Kafka cluster, a @deftech{Workspace Window} is
opened for that cluster.  All operations within the workspace operate
on the same connection.  When you close a Workspace Window, all of its
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
streaming new data into the table.  You can stop this by pressing the
``Toggle Live Mode'' button on the bottom left corner of the table.
You can configure how much data is requested from the topic on each
fetch by click the ``Options...'' button in the bottom right, and you
can manually load more data by pressing the ``Load More Records...''
button.

From the ``Options...'' popover, you can also reset the topic iterator
to any offset you like.

You can right-click on any record with a non-null key to publish a
tombstone for it.  Additionally, you can drag and drop any non-null
key or value from the table to any application that accepts files to
export the dragged value.  You can use the ``Key Format'' and ``Value
Format'' options from the ``Options...'' popover to control what
format the columns are exported as.

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
@tech{scripting window} toolbar or use the @kbd[cmd R] keyboard
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
``Reset...'' button to get to the @deftech{Reset Popover}.  From
there, you can reset the record iterator to various offsets, as
described below.

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

@subsubsub*section{Latest}

Queries each partition for its latest offset and moves the iterator
forward.

@subsubsub*section{Offset}

Moves the iterator to the given offset for every partition.  For
partitions that are behind the selected offset, no new data will be
received until they reach it.


@subsubsection{Configuration Table Tab}

The @deftech{Configuration Table} tab displays the selected topic's
configuration.  Non-default values are presented in bold and sensitive
values are hidden by default.  You may reveal sensitive values by
right clicking on them and pressing the ``Reveal'' context menu item.

@subsection{Consumer Groups}

When you select a consumer group from the @tech{Workspace Window}
sidebar, you are presented with the @deftech{Consumer Offsets Table}.
There, you can see member assignments, offsets and lag as well as
reset individual offsets by right-clicking any of the entries.

You may only reset offsets if the consumer group is in the empty
state.

@section{Keyboard Shortcuts}

@defkbd[(shift cmd 1)]{
  Display the @tech{Welcome Screen}.
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
  With a topic selected, switches to the Configuration tab.
}

@defkbd[(cmd R)]{
  Within a @tech{Workspace Window}, reloads the connection metadata.
  Within a @tech{Scripting Window}, activates or deactivates the
  script.
}

@section{Known Issues and Limitations}

Franz is currently in Beta so you should expect minor issues and
limitations to pop up.  I do my best to resolve these issues when they
appear, so please let me know when you run into them by emailing me at
@tt{bogdan@"@"defn.io} with the topic ``Franz Support''.

@subsection{Compression}

The only compression format currently supported is @tt{gzip}.  If you
need support for any other format, please e-mail me and let me know.

@section[#:tag "ref"]{Scripting Reference}

@(define-syntax-rule (deflua id (arg ...) res pre-content ...)
   (let ([id-str (symbol->string 'id)]
         [args-str (string-join (map symbol->string '(arg ...)) ", ")])
     (tabular
      #:style 'boxed
      #:column-properties '(left right)
      #:row-properties '(bottom-border ())
      (list
       (list @tt[@|id-str| "(" @args-str ")"] @tt[@symbol->string['res]])
       (list @elem[(elemtag `("lua"  ,id-str)) pre-content ...] 'cont)))))

@deflua[json.decode (str) table]{
  Decodes the JSON data in @tt{str} to a Lua table.
}

@deflua[json.encode (t) string]{
  Encodes the Lua table @tt{t} to JSON.
}

@deflua[math.abs (n) number]{
  Returns the absolute value of @tt{n}.
}

@deflua[math.ceil (n) number]{
  Rounds @tt{n} towards positive infinity.
}

@deflua[math.floor (n) number]{
  Rounds @tt{n} towards negative infinity.
}

@deflua[math.min (n ...) number]{
  Returns the smallest number amongst the given set.
}

@deflua[math.max (n ...) number]{
  Returns the largest number amongst the given set.
}

@deflua[math.sqrt (n) number]{
  Returns the square root of @tt{n}.
}

@deflua[string.sub (str i j) string]{
  Returns a substring of @tt{str} starting from @tt{i} until @tt{j}.
  The @tt{i} argument defaults to @racket[1] and the @tt{j} argument
  defaults to the length of @tt{str}.
}

@section{Credits}

@(define (racket-anchor . text)
  (apply link "https://racket-lang.org/" text))

Franz is built using the @racket-anchor{Racket programming language}
and distributes its runtime alongside the application.  Racket is
licensed under the MIT License.
