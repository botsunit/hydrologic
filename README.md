

# Hydrologic - A pipeline system #

Copyright (c) 2017 Grégoire Lejeune, 2017 Botsunit

__Version:__ 0.0.1

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)).


### CORE ###

`duplicate`

`{merge, Function :: function()} | {merge, Module :: atom(), Function :: atom()`

`fanin | {fanin, 0} | {fanin, 1}`


### STDLIB ###

`console` (map)

`{console, [Format :: string()]}` (map)

`{match, [Regex :: string()]}` (filter)

`{pad, [Direction :: left | right, Size :: integer(), Char :: integer()]} | {pad, [Size :: integer(), Char :: integer()]}` (map)

`{chop, [Size :: integer()]}` (map)

`{between, [Min :: string(), Max :: string()]}` (filter)

`odd` (filter)

`even` (filter)


### Create a worker ###

```

-spec worker(data()) -> {map, data()}
                        | data()
                        | {filter, true | false}
                        | {return, data()}
                        | {error, term()}.

```


### Examples ###

![](http://yuml.me/diagram/scruffy;dir:LR/class/%2F%2F Cool Class Diagram, [a|X rem 2 == 0|b]->[b|X + 2], [a|X rem 2 == 0|b]->[X * 2], [X * 2]->[b|X + 2])

```

hydrologic:new(
  test,
  [
   {a, {fun(X) ->
            {filter, X rem 2 == 0}
        end, b}},
   fun(X) ->
       {map, X * 2}
   end,
   {b, fun(X) ->
           {map, X + 2}
       end},
   return
  ]
 ),
hydrologic:run(test, 10), % => 22
hydrologic:run(test, 7). % => 9

```


### Licence ###

Hydrologic is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2017 Grégoire Lejeune<br />
Copyright (c) 2017 BotsUnit<br />

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.



THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


### TODO ###
* CORE: sort - (map, list)
* CORE: count lines|chars|words - (map, list)
* CORE: from fun(X) -> true|false end - (map, list)
* CORE: to fun(X) -> true|false end - (map, list)
* CORE: unique - (map, list)
* CODE: head N - (map, list)
* CODE: tail N - (map, list)
* CODE: drop head|tail N - (map, list)
* CODE: split Pattern - (map, elem->list)
* CODE: join Separator - (map, list->elem)
* STDLIB: strip left|right|both - (map, elem)
* STDLIB: replace Regex Repl - (map, elem)


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/botsunit/hydrologic/blob/master/doc/hydrologic.md" class="module">hydrologic</a></td></tr>
<tr><td><a href="https://github.com/botsunit/hydrologic/blob/master/doc/hydrologic_stdlib.md" class="module">hydrologic_stdlib</a></td></tr></table>

