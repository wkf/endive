(ns endive.core-test
  (:refer-clojure :exclude [read-string])
  (:require [clojure.test :refer :all]
            [endive.core :refer :all]
            [garden.units :as units]
            [garden.color :as color]
            [garden.stylesheet :as stylesheet]))

(deftest test-read-string
  (testing "read-string uses unit readers"
    (are [s r] (= (read-string s) r)
      "#px 25" (units/px 25)
      "#kHz 25" (units/kHz 25)))
  (testing "read-string uses color readers"
    (is (= (read-string "#rgba [255 255 255 0]") (color/rgba 255 255 255 0))))
  (testing "read-string uses at-rule readers"
    (is (= (read-string "#at-media [:small [:body {:color :blue}]]")
          (stylesheet/at-media
            :small
            [:body
             {:color :blue}])))))

(deftest test-selector-parse-and-render
  (testing "equivalent output using both parse-selectors and render-selectors"
    (are [i] (= i (-> i parse-selectors render-selectors))
      "div"
      "* > *::first-letter > div:nth-child(n + 1) #hat.bat")))

(deftest test-rule-expand-collapse
  (testing "equivalent output using both expand-rules and collapse-rules"
    (are [i] (= i (-> i expand-rules collapse-rules))
      [["html" {"padding" (units/px 25)}]])))

(def test-at-rules
  [["body#id.class::after" {:padding "24px"}]])

(deftest test-at
  (testing "at selector types"
    (are [c f] (f (at (expand-rules test-at-rules) (constantly nil) c identity))
      [:&#id] not-empty
      [:#id] not-empty
      [:body] not-empty
      [:.class] not-empty
      [:&.class] not-empty
      [:body.class] not-empty
      ["#id"] not-empty
      ["body"] not-empty
      ["&.class"] not-empty
      ["body::after"] not-empty
      [(property? :padding)] not-empty
      [(property? :padding "24px")] not-empty
      [(property? :margin)] empty?
      [(property? :padding "25px")] empty?
      [[:body :.class]] not-empty)))

(def test-transforms-rules
  [["body#id.class::after" {:padding "24px"}]
   (stylesheet/at-media {:snap :small}
     [:div
      {:margin "22px"}])])

(deftest test-transforms
  (testing "transform types"
    (are [t p v] (= v
                   (get-in
                     (vec
                       (at
                         (expand-rules test-transforms-rules)
                         (constantly nil) [:&] t))
                     p))
      (set-property :padding "25px") [0 1 0 "padding"] "25px"
      (remove-property :padding) [0 1 0 "padding"] nil
      (set-property :margin "25px") [0 1 1 "margin"] "25px"
      (remove-class :.class) [0 0 0 3] nil
      (splice
        (fn [] (expand-rules test-transforms-rules))) [1 1 0 "padding"] "24px"
      (splice
        (fn [_] (expand-rules test-transforms-rules)) :padding) [0 1 0 "padding"] nil
        (replace-value "24px" "25px") [0 1 0 "padding"] "25px"))
  (testing "at-media transform"
    (are [c t p v] (= v
                     (get-in
                       (vec
                         (at (expand-rules test-transforms-rules) (constantly nil) c t))
                       p))
      (at-media? {:snap :small}) (at-media {:min-width "22px"})
      [0 :value :media-queries :min-width] "22px"
      (at-media? {:snap :small} [:&]) (set-property :padding "21px")
      [0 :value :rules] '([[[{:type :element, :value "div"}]]
                           [{"margin" "22px"} {"padding" "21px"}]]))))

;;; "Integration" tests

(defsnippet test-link "style.edn" [:.my-link] [[color height]]
  [:&] (remove-class :.my-link)
  ["&::before"] (set-property
                  :color color
                  :height height))

(deftemplate test-styles "style.edn" []
  [:.my-link] (constantly nil)
  (at-media? {:snap :small}) (at-media {:min-width "1234px"})
  [(property? :my-link)] (splice my-link :my-link)
  [:&] (replace-value :blueish "#11aaff"))

(deftest test-defsnippet
  (let [s (test-link [:blue "1px"])]
    (testing "remove-class from snippet"
      (is (empty? (get-in (nth s 0) [0 0]))))
    (testing "set-property in snippet"
      (is (= :blue (get-in (nth s 1) [1 0 "color"]))))))

(deftest test-deftemplate
  (let [t (test-styles)]
    (testing "at-media in template"
      (is (= "1234px" (get-in (nth t 5) [:value :media-queries :min-width]))))))
