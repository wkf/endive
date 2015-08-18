# Endive

Endive is a CSS selector-based templating library built with [Garden](https://github.com/noprompt/garden) and the straightforwardly named [CSS Parser](http://cssparser.sourceforge.net/). It borrows a lot of ideas from [Enlive](https://github.com/cgrand/enlive) and [PostCSS](https://github.com/postcss/postcss). Most importantly, instead of mixing logic and styles, styles are represented as pure edn-formatted data. In order to add logic, you write transformations (in Clojure) over the parsed CSS. The syntax and terminology is very similar to Enlive.

That's the idea anyway. Most things work, but this is still very alpha software.

## Installation

To install, add the following dependency to your project.clj file:

[![Clojars Project](http://clojars.org/endive/latest-version.svg)](http://clojars.org/endive)

## Usage ##

To start using Endive, require `endive.core`.

```clojure
(ns my.great.namespace
  (:require [endive.core :as css :refer css]))
```

### Writing your styles ###

Currently, the only parser expects styles to be in edn-formatted files. The syntax is much like Garden, with reader macros for units and @rules, like so:

```clojure
[[:body
  {:padding #px 25}]

 #at-media
 [{:snap :small}
  [:div
   {:background :blue}
   ["&::first-line"
    {:color :green}]]]]
```

Notice the `#px` reader macro. Once read, this is expanded to a garden pixel unit. There are macros for most CSS units, but you're free to use strings as well: `"25px"`. Notice also the `#at-media` macro. This should precede a vector holding media-queries and any affected rules. Much like the unit macros, this is expanded to a Garden `AtRule` record at read time.


### Snippets and Templates

Much like Enlive, styles are broken up into snippets and templates. Snippets usually represent part of a larger document, while templates act more like "layouts". Both serve as ways to "celect "and transform styles. Here are some examples of how one might define snippets and templates with the `defsnippet` and `deftemplate` macros respectively. Provide a name, resource, bindings vector and any number of celector/transform pairs to create snippet or template functions. In the case of snippets, you'll also need to provide a scoping celector that normally matches only a subset of rules from a document. Calling a snippet function returns a seq of parsed rules, whereas calling a template function returns a seq of rules in a format suitable for `garden.core/css`. This is the primary way to actually emit CSS. Keep reading to learn more about how the celectors and transforms work.

```clojure
(defsnippet my-link "style.edn" [:.my-link] [[color height]]
  [:&] (remove-class :.my-link)
  [[:body :.class]] identity
  ["&::before"] (set-property :color color :height height))

(deftemplate my-styles "style.edn" []
  [:.my-link] (constantly nil)
  (at-media? {:snap :small}) (at-media {:min-width (units/px 1234)})
  [(property? :my-link)] (splice my-link :my-link)
  [:&] (replace-value :blueish "#11aaff"))
```

### "Celectors"

Note: In order to avoid confusion (or maybe add more) when writing this library, I chose to call selectors that select CSS selectors celectors. This could change at any time, but bear with me for now.

Celectors look and act mostly like CSS selectors, and (like most of this library) syntax is pretty similar to Enlive. You can use keywords or strings to represent ids, classes, elements, pseudo-classes, and pseudo-elements. You can use built-in predicates to match property names and values. You can also write your own predicates to match on any part of a parsed CSS rule.

Example celectors:

```clojure

;; match rules targeting an element with id #id
["#id"]

;; match rules targeting a class .class under a body element
[:body :.class]

;; match rules targeting a body element with a class .class
[[:body :.class]]

;; match rules mentioning padding
[(css/property? :padding)]

;; match rules mentioning padding of "24px"
[(css/property? :padding "24px")]

;; match the actual @media rule
;; ...this allows changing things like media queries
(at-media? {:snap :small})

;; match rules with specific media queries
(at-media? {:snap :small} [:body])

```


### Transforms

Once you've celected some rules, you'll likely want to act on them. Endive provides a number of built-in transformations, but you can also build your own, as they are just functions take a rule and returning either a rule or set of rules.

Example transforms:

```clojure

;; add a property to a rule
(set-property :padding "25px")

;; remove a property from a rule
(remove-property :padding)

;; search for and replace a value with another value
;; ... useful for creating meaningful constants
(replace-value "24px" "25px")

;; remove a class from the selectors associated with a rule
(remove-class :.class)

;; "splice" a set of rules into the current rule
;; ... very useful combined with remove-class to create "generic" rules
(splice
  (fn [_] (expand-rules test-transforms-rules)) :padding)

;; set media queries for an @media rule
;; ... *must* target an @media rule
(at-media {:min-width "22px"})

```

### Emitting CSS

The easiest way to emit CSS is to use `garden.core/css` on the output of a template function.

```core
(garden.core/css (my-template))
```

Happy styling!

## License ##

Copyright © 2015 [Will Farrell](http://willfarrell.is)

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
