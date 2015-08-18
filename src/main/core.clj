(ns endive.core
  (:refer-clojure :exclude [read-string])
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [garden.core :refer [css]]
            [garden.color :as color]
            [garden.units :as units]
            [garden.stylesheet :as stylesheet])
  (:import (jodd.csselly CSSelly CssSelector Combinator Selector$Type)
           (jodd.csselly.selector PseudoClass PseudoClassSelector)

           (com.helger.css.reader CSSReader)
           (com.helger.commons.charset CCharset)
           (com.helger.css ECSSVersion)

           (cz.vutbr.web.css CSSFactory)

           (java.io StringReader)
           (org.w3c.css.sac InputSource)
           (com.steadystate.css.parser CSSOMParser)

           ))

;; # TODO
;;
;;   - [x] Read styles from edn file
;;   - [ ] Expand rules to a format that's easier to search
;;   - [ ] `at' form to select and transform rules
;;   - [ ] Selection helpers (prop?, at-media?)
;;   - [ ] Transform  helpers (selectors, props, values)
;;   - [ ] Serialize modified rules (to garden format, and then css)
;;   - [ ] Macros to create snippets and templates
;;

(def readers
  {'in units/in
   'cm units/cm
   'pc units/pc
   'mm units/mm
   'pt units/pt
   'px units/px
   'percent units/percent
   'deg units/deg
   'grad units/grad
   'rad units/rad
   'turn units/turn
   's units/s
   'ms units/ms
   'Hz units/Hz
   'kHz units/kHz
   'dpi units/dpi
   'dpcm units/dpcm
   'dppx units/dppx

   'rgb color/rgb
   'rgba color/rgba
   'hsl color/hsl
   'hsla color/hsla

   'at-media #(apply stylesheet/at-media %)
   'at-import #(apply stylesheet/at-import %)
   'at-font-face #(apply stylesheet/at-font-face %)
   'at-keyframes #(apply stylesheet/at-keyframes %)})

(defn read-string [string]
  (edn/read-string {:readers readers} string))

(defn read-resource [path]
  (->> path io/resource slurp read-string list*))

(defn parse-component [component]
  (condp #(= %1 (.getType %2)) component
    Selector$Type/ATTRIBUTE {:type :attribute
                             :match (some-> component .getMatch .getSign)
                             :name (.getName component)
                             :value (.getValue component)}
    Selector$Type/PSEUDO_CLASS {:type :pseudo-class
                                :name (some-> component .getPseudoClass .getPseudoClassName)}
    Selector$Type/PSEUDO_FUNCTION {:type :pseudo-function
                                   :name (some-> component .getPseudoFunction .getPseudoFunctionName)
                                   :expression (.getExpression component)}
    {:type :unknown}))

(defn parse-components [components]
  {:element (.getElement components)
   :combinator (some-> components .getCombinator .getSign)
   :components (->>
                 (range (.selectorsCount components))
                 (map #(.getSelector components %))
                 (map parse-component))})

(defn parse-selector [selector]
  (->> selector CSSelly. .parse (map parse-components)))

(defn expand-rule [rule]
  (if (record? rule)
    rule
    (let [[selectors declarations] rule]
      [(->> selectors
         (map (partial str/join " "))
         #_(map parse-selector))
       declarations])))

(defn expand-rules [rules]
  (->> rules (#'garden.compiler/expand-stylesheet) (map expand-rule)))




(comment
  ;; public static class MyPseudoClass extends PseudoClass
  ;; @Override
  ;; public boolean match(Node node) {
  ;;                                  return node.hasAttribute("jodd-attr");
  ;;                                  }

  ;; @Override
  ;; public String getPseudoClassName() {
  ;;                                     return "some-cool-name";
  ;;                                     }

  ;; PseudoClassSelector.registerPseudoClass(MyPseudoClass.class);

  (PseudoClassSelector/registerPseudoClass
    (get-proxy-class
      (proxy [PseudoClass] []
        (getPseudoClassName [_] "after"))))


  (expand-rules
    (read-resource "style.edn"))

  )


(defn parse-path [path]
  (->> path (map name) (str/join " ")))

(defn match [nodes selector]
  ;;
  )

(comment
  (normalize "&.hello")
  (normalize :&.hello)
  (normalize "div")
  )

(defn selects?
  "Predicate returning `true' if `node' is matched by `selector'.

  Valid selectors:

  :&           ; select anything
  \"&\"

  :#id         ; select by id
  :&#id
  \"#id\"
  \"&#id\"

  :.class      ; select by class
  :&.class
  \".class\"
  \"&.class\"

  :&:before    ; select any with before pseudo-element
  \":before\"
  \"&:before\"

  #{sel1 sel2}  ; logical or
  [[sel1 sel2]] ; logical and

  [sel1 sel2]   ; ordered

  [sel1 :> sel2]

  "
  [rule selector]

  (let [selector (name selector)]

    (count "&hello")
    )

  ;; (name selector)
  (first rule)

  )

(defn at [nodes & rules]
  (reduce
    (fn [nodes [selector transform]]
      (mapcat
        (fn [node]
          (if (selects? node selector)
            (transform node)
            [node]))
        nodes))
    nodes
    (partition 2 rules)))

(comment


  (CSSReader/readFromString
    ".hello .head:nth-child(n + 1){}" CCharset/CHARSET_UTF_8_OBJ ECSSVersion/CSS30)

  (CSSFactory/parseString
    ".hello .head:nth-child(n + 1)" nil)

  InputSource source = new InputSource(new StringReader("h1 { background: #ffcc44; }"));
  CSSOMParser parser = new CSSOMParser(new SACParserCSS3());

  (CSSOMParser/parseSelectors
    (InputSource. (StringReader. ".hello")))


  (def reset
    [[:*
      {:background [[:none :repeat :scroll 0 0 :transparent]]
       :border [[:medium :none]]
       :border-spacing 0
       :color "black"
       :font-weight :normal
       :list-style [[:none :outside :none]]
       :margin 0
       :padding 0
       :text-align :left
       :text-decoration :none
       :text-indent 0}]
     [:li
      {:list-style-type :none}]
     ["input::-moz-focus-inner"
      "button::-moz-focus-inner"
      {:border 0
       :padding 0}]
     [:button:focus
      {:outline 0}]])

  ;; flatten garden(ish) styles to make searching easier
  ;; write a "selector-selector" dsl to search the flattened styles

  (parse-selector ".hello .head:nth-child(n + 1)")

  (parse-selector ".hello[lang|=en] .head:nth-child(n + 1)")

  (parse-selector "@media (screen)")

  (->> (read-resource "style.edn")
    (#'garden.compiler/expand-stylesheet))

  (#'garden.compiler/expand-stylesheet
    (list* [[:html :body {:padding (units/px 25)}]]))

  (#'garden.compiler/expand-rule
    [:html :body {:padding (units/px 25)}])

  (css [[:html :body {:padding (units/px 25)}]])


  :&

  (->
    [:.hello ".butts:nth-child(n + 1)"]
    parse-path
    parse-selector)

  [:... [:.buns :.crumpets] :...]

  (->>
    (io/resource "style/nav.edn")
    slurp
    (edn/read-string {:readers {'px px}})
    flatten-node
    (map #(update-in % [0] parse-path))

    )

  ;; :#id
  ;; :.class
  ;; ":pseudo-class"
  ;; ":pseudo-function"

  ;; need a way to strip some components
  (defsnippet link "style/link.edn"
    [:.link] [color height]
    [css/any] (css/components-after :.link)
    [":before"] (css/set-property
                  :color color
                  :height height))

  (defsnippet nav "style/nav.edn" [css/any] [])
  (defsnippet header "style/header.edn" [css/any] [])
  (defsnippet footer "style/footer.edn" [css/any] [])

  (deftemplate main "style/main.edn" [& snippets]

    [:...] (apply css/append snippets)

    [:...] (transform-val :green green)

    [:...] (splice :sze-architects/link link)
    [:...] (splice :sze-architects/underline underline)

    [(at-media? :sze-architects/medium)] (at-media {:min-width (px 768)})
    [(at-media? :sze-architects/large)] (at-media {:min-width (px 1190)}))


  (css config (main (header) (footer) (nav)))

  ;; any rule targetting a div

  [:* :div :*]

  ;; any rule containing


  (defsnippet link "" )

  (a-link (px 1) "#ffffff")

  (a-link)

  (def styles
    (->
      (read-styles "style/nav.edn")
      (transform-values
        :sze-architects/green "#ff0000")
      (transform-media
        :sze-architects/medium {:min-width (px 768)}
        :sze-architects/large {:min-width (px 1190)})
      (transform-declarations
        :sze-architects/link (fn [[height color]] ...)
        :sze-architects/underline (fn [[height color]] ...))))


  (def z
    (->>
      (io/resource "style/nav.edn")
      slurp
      (edn/read-string
        {:readers
         {'px px
          'at-media #(apply at-media %)}})

      (walk/prewalk
        (fn [node]
          (let [matches {:sze-architects/link {:crumpet :muffin}
                         :sze-architects/underline [:&
                                                    {:butt :head}
                                                    [:&:before
                                                     {:buns :candy}]]}]
            (if (vector? node)
              (mapv
                (fn [m]
                  (if (and (map? m) (not (record? m)))
                    (let [[props rules]
                          (reduce
                            (fn [[props rules] [k v]]
                              (if-let [match (matches k)]
                                (if (map? match)
                                  [(merge props match) rules]
                                  [props (conj rules match)])
                                [(assoc props k v) rules]))
                            [{} []] m)]
                      (cond
                        (empty? rules) props
                        (empty? props) (apply vector :& rules)
                        :else (apply vector :& props rules)))
                    m))
                node)
              node))))


      #_(walk/prewalk
          (fn [node]
            (let [matches {:sze-architects/green "#ffffff"}]
              (if (and (map? node) (not (record? node)))
                (->>
                  node
                  (map (fn [[k v]]
                         (println k v)
                         [k (cond
                              (and
                                (vector? v)
                                (vector? (first v))) [(mapv #(get matches % %) (first v))]
                                (vector? v) (mapv #(get matches % %) v)
                                :else (get matches v v))]))
                  (into {}))
                node))))

      #_(walk/prewalk
          (fn [node]
            (if (and
                  (instance? garden.types.CSSAtRule node)
                  (-> node :identifier #{:media}))
              (update-in node [:value :media-queries]
                (partial get {:sze-architects/medium {:min-width (px 768)}
                              :sze-architects/large {:min-width (px 1100)}}
                  (-> node :value :media-queries)))
              node)))



      ))

  (walk/prewalk-replace
    {:sze-architects/medium {:min-width (px 768)}
     :sze-architects/large {:min-width (px 1100)}})


  (at x

    ;; select rules?
    ;; separate functions for rules/selectors/declarations/properties/values?


    ;; treat keywords as property names
    :sze-architects/link (fn [zipper] ;; a zipper??
                           (edit zipper (fn [[name value]]))

                           )

    [[:.class ]] (css/substitute-rule nil)


    ;; first, scope to some set of rules
    (select-rule :#id)
    (select-rule :.class)
    (select-rule [:.class (prop? :prop)])
    (select-rule [[:.class (prop? :prop)]])


    :* (css/substitute-val :my/val
         "another-value")

    :* (css/substitute-prop :my/prop
         {:another-prop "and-a-value"})

    :* (css/substitute-decl

         )


    (val? :my/val) (css/substitute-val :my/val "blue")
    (prop? :my/prop) (css/substitute-prop :my/prop
                       {:hello "buns"})


    (select-selector :my/selector)

    (select-declaration )

    (select-property )

    (select-value)


    (prop-val? :value-a) (css/replace-val :property-a
                           )

    (prop-name? :property-a) (css/replace-prop :property-a
                               {:something :else})


    (css/replace-property :property-a



      )


    :property-a (css/substitute-rule nil)

    [:selector-a :property-a] (css/substitute-property )

    [:selector-a #{:property-a
                   :property-b}]


    (css/selector)
    (css/declaration :sze-architects/link)


    (css/declaration :sze-architects/link)


    [(css/rule :.show-menu)
     (css/declaration :sze-architects/link)] (fn [[[height color]]]
                                               ;; receive the declaration value...
                                               ;; ...return either a property map or a vector of rules
                                               )

     (css/value :sze-architects/green) (css/substitute green)
     (css/media :sze-architects/medium) (css/substitute
                                          {:screen true
                                           :min-height (px 1234)})
     (css/selector :.buns)
     (css/declaration :sze-architects/link)


     :sze-architects/link (fn [[height color]]


                            )


     (css/substitute
       {:cursor :pointer
        :position :relative
        :transition [[:all "200ms" :ease]]}
       [:&:before
        {:content "''"
         :position :absolute
         :left 0
         :right 0
         :bottom 0
         :height (px 1)
         :background white
         :transform "translateY(0)"
         :transition [[:all "200ms" :ease]]}]
       [:&:hover
        [:&:before
         {:transform "translateY(1px)"}]])

     :sze-architects/underline

     )


  )
