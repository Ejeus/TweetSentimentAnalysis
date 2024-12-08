#lang racket
;EMMANUEL EJEU
;2024/HD05/21922U
;2400721922

;;; Import required libraries
(require csv-reading)                  ; For reading CSV files
(require data-science)                 ; For sentiment analysis
(require plot)                         ; For visualization
(require racket/string)                ; For text manipulation

;;; 1. Read Tweets from CSV File
(define (read-tweets file-path)
  "Reads the CSV file and returns a list of (created_at, text, location)."
  (map (lambda (row) (list (first row) (second row) (third row)))
       (rest (read-csv file-path))))  ; Skip header row

(define tweet-data (read-tweets "tweets.csv"))  ; Replace with the path to your CSV file

;;; 2. Filter Tweets by Location
(define (filter-tweets-by-location tweets location)
  "Filters tweets based on the specified location."
  (filter (lambda (row)
            (and (string? (third row))
                ; (string-ci=? (third row) location)))
                ; (string-ci-contains? (string-trim (third row)) location)))  ; Check for "Canada"
                  (regexp-match? (regexp (string-downcase location))          ; Match lowercase location
                               (string-downcase (string-trim (third row)))))) ; Lowercase and trim
          tweets))

(define canada-tweets (filter-tweets-by-location tweet-data "Canada"))

(printf "Number of filtered tweets: ~a\n" (length canada-tweets))
;(printf "Number of formatted tokens: ~a\n" (length formatted-tokens))
;(printf "Number of sentiment entries: ~a\n" (length sentiment))
;(for ([entry (take formatted-tokens 50)]) (printf "~a\n" entry)) ; Display first 50 tokens
;Sentiment: ((explain positive 1) (explain trust 1) (beauty joy 1) ...)


(if (empty? canada-tweets)
    (printf "No tweets found for the specified location.\n")
    (printf "Filtered tweets: ~a\n" (take canada-tweets 5)))
   ; (begin
   ;   (printf "Filtered Tweets:\n")
   ;   (for ([tweet canada-tweets]) (printf "~a\n" tweet)))) ;does whole dataset

;;; 3. Extract and Normalize the 'text' Column
(define tweet-texts (map second canada-tweets))  ; Extract the 'text' column

(if (empty? tweet-texts)
    (printf "No tweet texts available for processing.\n")
    (printf "Tweet texts: ~a\n" (take tweet-texts 5)))

(define normalized-texts
  (map (lambda (text)
         (string-normalize-spaces (remove-punctuation (string-downcase text))))
       tweet-texts))

;;; Tokenize and Prepare Tokens for Sentiment Analysis
(define raw-tokens
  (apply append (map (lambda (text) (document->tokens text #:sort? #t)) normalized-texts)))

;;; Format tokens for sentiment analysis
(define formatted-tokens
  (filter
   pair?
   (map (lambda (pair)
          (if (pair? pair)
              pair  ; Keep valid pairs as is
              (list pair "dummy")))  ; Add dummy metadata for single tokens
        raw-tokens)))

(if (empty? formatted-tokens)
    (printf "No tokens extracted from tweets.\n")
    (printf "Formatted Tokens: ~a\n" (take formatted-tokens 10)))

;;; 4. Perform Sentiment Analysis
(define raw-sentiment
  (list->sentiment formatted-tokens #:lexicon 'nrc))  ; Pass the properly formatted token list
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remove Header or Invalid Entries
(define sentiment
  (filter (lambda (entry)
            (and (list? entry)                     ; Ensure it's a list
                 (>= (length entry) 3)             ; Ensure at least 3 elements
                 (string? (list-ref entry 0))      ; First element is a string
                 (string? (list-ref entry 1))      ; Second element is a string
                 (number? (list-ref entry 2))))    ; Third element is a number
          raw-sentiment))
;;;;;;;;;;;;;;;;;;;;;;;;
(if (empty? sentiment)
    (printf "No sentiment data available.\n")
    ;(printf "Sentiment: ~a\n" sentiment) ;The whole dataset
    (printf "Sentiment: ~a\n" (take sentiment 10)))

;;; 5. Aggregate Sentiment Labels
(define (aggregate-sentiment sentiment-data)
  "Aggregates sentiment counts into a list of (label count) pairs."
  ;; Filter valid entries from sentiment-data
  (define valid-sentiments
    (filter
     (lambda (entry)
       (and (list? entry)                 ; Ensure it's a list
            (>= (length entry) 3)         ; Ensure it has at least 3 elements
            (string? (list-ref entry 1))  ; 'sentiment' is a string
            (number? (list-ref entry 2)))) ; 'freq' is a number
     sentiment-data))
  
  ;; Debugging: Print invalid entries if any
  (define invalid-sentiments
    (filter (lambda (entry) (not (member entry valid-sentiments))) sentiment-data))
  (when (not (empty? invalid-sentiments))
    (printf "Warning: Found invalid sentiment entries: ~a\n" invalid-sentiments))

  ;; Aggregate valid sentiments using a mutable hash
  (define sentiment-groups (make-hash))  ; Create a mutable hash table
  (for ([entry valid-sentiments])
    (define label (list-ref entry 1))    ; Access 'sentiment'
    (define freq (list-ref entry 2))     ; Access 'freq'
    (hash-set! sentiment-groups
               label
               (+ (hash-ref sentiment-groups label 0) freq)))  ; Increment frequency
  
  ;; Convert hash to list of (label count) pairs
  (for/list ([key (in-hash-keys sentiment-groups)])
    (list key (hash-ref sentiment-groups key))))


(define sentiment-aggregation (aggregate-sentiment sentiment))

;;; 6. Visualize Sentiment Distribution
(define (plot-sentiment-distribution sentiment-data)
  "Plots the sentiment distribution as a barplot."
  (if (empty? sentiment-data)
      (printf "No sentiment data available for plotting.\n")
      (parameterize ((plot-width 800))
        (plot (list
               (tick-grid)
               (discrete-histogram
                (sort sentiment-data (λ (x y) (> (second x) (second y))))
                #:color "MediumSlateBlue"
                #:line-color "MediumSlateBlue"))
              #:x-label "Affective Label"
              #:y-label "Frequency"
              #:title "Sentiment Distribution for Canada"))))

(plot-sentiment-distribution sentiment-aggregation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Aggregate Sentiments by Category
(define (aggregate-sentiment-contributions sentiment-data)
  "Aggregates word contributions to each sentiment."
  (define sentiment-groups (make-hash)) ; Create a mutable hash table
  (for ([entry sentiment-data])
    (define word (first entry))         ; Extract the word
    (define sentiment (second entry))  ; Sentiment category
    (define freq (third entry))         ; Word frequency
    (define current (hash-ref sentiment-groups sentiment '())) ; Get current list
    (hash-set! sentiment-groups sentiment (cons (list word freq) current))) ; Update
  sentiment-groups)

(define aggregated-contributions (aggregate-sentiment-contributions sentiment))
(printf "Aggregated Contributions: ~a\n" aggregated-contributions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sentiment Contribution Percentage: Display each sentiment category’s percentage contribution.
(define (sentiment-percentage sentiment-groups)
  "Calculates the percentage contribution of each sentiment."
  (define total (apply + (for/list ([sentiment (hash-keys sentiment-groups)])
                          (apply + (map second (hash-ref sentiment-groups sentiment))))))
  (for ([sentiment (hash-keys sentiment-groups)])
    (define count (apply + (map second (hash-ref sentiment-groups sentiment))))
    (printf "Sentiment: ~a, Percentage: ~a%%\n"
            sentiment
            (* (/ count total) 100))))

(sentiment-percentage aggregated-contributions)

;Due to lack of time, and given the scope of the question that limited what we could do with the data, ;
;I was not able to do the analysis below but would have loved to add them to my project
;I encourage other students to build on these for deeper analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Heatmap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Pie

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sentiment by word length
(define (group-sentiments-by-word-length sentiment-data)
  "Groups sentiments by word length."
  (define word-length-sentiments (make-hash))
  (for ([entry sentiment-data])
    (define word (first entry))
    (define sentiment (second entry))
    (define freq (third entry))
    (define length (string-length word)) ; Get word length
    (define current (hash-ref word-length-sentiments length (make-hash))) ; Get existing or initialize
    (hash-set! current sentiment (+ (hash-ref current sentiment 0) freq)) ; Increment count
    (hash-set! word-length-sentiments length current)) ; Update main hash
  word-length-sentiments)

(define (plot-sentiments-by-word-length word-length-sentiments)
  "Plots sentiment counts by word length as grouped discrete histograms."
  (define lengths (sort (hash-keys word-length-sentiments) <))
  (define sentiments '(positive negative neutral))
  (define histograms
    (for/list ([sentiment sentiments])
      (discrete-histogram
       (for/list ([length lengths])
         (vector length (hash-ref (hash-ref word-length-sentiments length (make-hash)) sentiment 0)))
       #:color (case sentiment
                 [(positive) 'green]
                 [(negative) 'red]
                 [(neutral) 'blue])
       #:label (symbol->string sentiment))))
  (parameterize ([plot-width 800])
    (plot histograms
          #:x-label "Word Length"
          #:y-label "Sentiment Frequency"
          #:title "Sentiment by Word Length")))

;;; Perform Analysis
(define word-length-sentiments (group-sentiments-by-word-length sentiment))
(plot-sentiments-by-word-length word-length-sentiments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sentiment over time


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Distribution by word length
(define (word-length-distribution token-list)
  "Groups tokens by word length and calculates frequencies."
  (define length-frequencies (make-hash))
  (for ([token token-list])
    (define length (string-length (first token))) ; Get word length
    (hash-set! length-frequencies
               length
               (+ (hash-ref length-frequencies length 0) (second token)))) ; Increment frequency
  length-frequencies)

(define (plot-word-length-distribution length-frequencies)
  "Plots the distribution of word lengths as a discrete histogram."
  (define lengths (sort (hash-keys length-frequencies) <)) ; Sorted word lengths
  (define counts (map (lambda (len) (hash-ref length-frequencies len)) lengths)) ; Corresponding counts
  (parameterize ([plot-width 800])
    (plot (list
           (discrete-histogram
            (for/list ([length lengths] [count counts])
              (vector length count))
            #:color 'blue))
          #:x-label "Word Length"
          #:y-label "Frequency"
          #:title "Distribution of Word Lengths")))

;;; Perform Analysis
(define length-frequencies (word-length-distribution formatted-tokens))
(plot-word-length-distribution length-frequencies)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Group Sentiments by Date
(define (group-sentiments-by-date sentiment-data tweets)
  "Groups sentiment counts by date (YYYY-MM-DD)."
  (define date-sentiments (make-hash)) ; Mutable hash table
  (for ([entry sentiment-data]
        [tweet tweets])
    (define created-at (first tweet)) ; Access the created_at field
    (when (and created-at (>= (string-length created-at) 10)) ; Ensure the date exists and is valid
      (define date (substring created-at 0 10)) ; Extract the date (YYYY-MM-DD)
      (define sentiment (string-downcase (second entry))) ; Normalize sentiment
      (define freq (third entry))       ; Frequency
      (define current (hash-ref date-sentiments date (make-hash))) ; Get or initialize date hash
      (hash-set! current sentiment (+ (hash-ref current sentiment 0) freq)) ; Increment count
      (hash-set! date-sentiments date current))) ; Update the main hash
  date-sentiments)

;;; Debugging Function
(define (debug-date-sentiments date-sentiments)
  "Prints the date-sentiments hash for debugging."
  (for ([date (sort (hash-keys date-sentiments) string<?)])
    (printf "Date: ~a, Sentiments: ~a\n"
            date
            (hash-ref date-sentiments date))))

;;; Plot Sentiments Over Time
(define (preprocess-date date)
  "Converts a date string (DD/MM/YYYY) into a numeric format (YYYYMMDD)."
  (let* ([parts (string-split date "/")] ; Split date into day, month, year
         [day (list-ref parts 0)]
         [month (list-ref parts 1)]
         [year (list-ref parts 2)])
    (string->number (string-append year month day)))) ; Reorder to YYYYMMDD

(define (plot-sentiments-over-time date-sentiments)
  "Plots sentiments over time as a line graph."
  (define dates (sort (hash-keys date-sentiments) string<?)) ; Sorted dates
  (when (empty? dates)
    (error "No valid dates found in the dataset.")) ; Handle empty data

  (define sentiments '(positive negative neutral)) ; Sentiment categories

  ;; Debugging: Print the data
  (for ([date dates])
    (printf "Date: ~a, Sentiments: ~a\n" date (hash-ref date-sentiments date)))

  ;; Generate plot lines
  (define lines-data
    (for/list ([sentiment sentiments])
      (lines
       (for/list ([date dates])
         (let* ([numeric-date (preprocess-date date)] ; Convert to numeric format
                [sentiment-value (hash-ref (hash-ref date-sentiments date (make-hash)) sentiment 0)])
           (if (and numeric-date (number? sentiment-value))
               (vector numeric-date sentiment-value)
               (vector 0 0)))) ; Default to 0 if invalid
       #:label (symbol->string sentiment)
       #:color (case sentiment
                 [(positive) 'green]
                 [(negative) 'red]
                 [(neutral) 'blue]))))

  ;; Debugging: Print `lines-data`
  (for ([line lines-data])
    (printf "Line Data for Sentiment: ~a\n" line))

  ;; Plot the data
  (parameterize ([plot-width 800])
    (plot (list* (tick-grid) lines-data)
          #:x-label "Date"
          #:y-label "Sentiment Frequency"
          #:title "Sentiment Trends Over Time")))

(define date-sentiments (group-sentiments-by-date sentiment canada-tweets))
(plot-sentiments-over-time date-sentiments)
