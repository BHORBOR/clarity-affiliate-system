;; product-manager
;;
;; This contract manages product listings with categories and promotional features
;; for the clarity-affiliate-system. It allows merchants to organize products into
;; categories, set featured status, apply discounts, create bundles, and track
;; performance metrics.
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PRODUCT-EXISTS (err u101))
(define-constant ERR-PRODUCT-NOT-FOUND (err u102))
(define-constant ERR-CATEGORY-EXISTS (err u103))
(define-constant ERR-CATEGORY-NOT-FOUND (err u104))
(define-constant ERR-BUNDLE-EXISTS (err u105))
(define-constant ERR-BUNDLE-NOT-FOUND (err u106))
(define-constant ERR-INVALID-DISCOUNT (err u107))
(define-constant ERR-INVALID-DATES (err u108))
(define-constant ERR-PRODUCT-NOT-IN-BUNDLE (err u109))
(define-constant ERR-PRODUCT-ALREADY-IN-BUNDLE (err u110))
;; Data structures
;; Product structure
(define-map products
  { product-id: uint }
  {
    name: (string-ascii 100),
    description: (string-utf8 500),
    price: uint,
    merchant: principal,
    created-at: uint,
    updated-at: uint,
    active: bool,
    featured: bool,
  }
)
;; Product to category mapping
(define-map product-categories
  {
    product-id: uint,
    category-id: uint,
  }
  { assigned-at: uint }
)
;; Category structure
(define-map categories
  { category-id: uint }
  {
    name: (string-ascii 50),
    description: (string-utf8 200),
    parent-category-id: (optional uint),
    created-at: uint,
    updated-at: uint,
  }
)
;; Discount promotions
(define-map product-discounts
  { product-id: uint }
  {
    discount-percentage: uint,
    start-time: uint,
    end-time: uint,
    created-at: uint,
  }
)
;; Product bundles
(define-map bundles
  { bundle-id: uint }
  {
    name: (string-ascii 100),
    description: (string-utf8 300),
    price: uint,
    merchant: principal,
    created-at: uint,
    updated-at: uint,
    active: bool,
  }
)
;; Mapping for products in bundles
(define-map bundle-products
  {
    bundle-id: uint,
    product-id: uint,
  }
  { added-at: uint }
)
;; Performance metrics
(define-map product-metrics
  { product-id: uint }
  {
    view-count: uint,
    affiliate-sales-count: uint,
    direct-sales-count: uint,
    revenue: uint,
    last-updated: uint,
  }
)
;; Counters for IDs
(define-data-var next-product-id uint u1)
(define-data-var next-category-id uint u1)
(define-data-var next-bundle-id uint u1)
;; Private functions
;; Get current block time
(define-private (get-current-time)
  block-height
)

;; Check if sender is the product merchant
(define-private (is-product-merchant (product-id uint))
  (let ((product-data (map-get? products { product-id: product-id })))
    (if (is-some product-data)
      (is-eq tx-sender (get merchant (unwrap-panic product-data)))
      false
    )
  )
)

;; Check if sender is the bundle merchant
(define-private (is-bundle-merchant (bundle-id uint))
  (let ((bundle-data (map-get? bundles { bundle-id: bundle-id })))
    (if (is-some bundle-data)
      (is-eq tx-sender (get merchant (unwrap-panic bundle-data)))
      false
    )
  )
)

;; Initialize metrics for a new product
(define-private (initialize-metrics (product-id uint))
  (map-set product-metrics { product-id: product-id } {
    view-count: u0,
    affiliate-sales-count: u0,
    direct-sales-count: u0,
    revenue: u0,
    last-updated: (get-current-time),
  })
)

;; Read-only functions
;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

;; Get category details
(define-read-only (get-category (category-id uint))
  (map-get? categories { category-id: category-id })
)

;; Get bundle details
(define-read-only (get-bundle (bundle-id uint))
  (map-get? bundles { bundle-id: bundle-id })
)

;; Check if product is in category
(define-read-only (is-product-in-category
    (product-id uint)
    (category-id uint)
  )
  (is-some (map-get? product-categories {
    product-id: product-id,
    category-id: category-id,
  }))
)

;; Check if product is in bundle
(define-read-only (is-product-in-bundle
    (product-id uint)
    (bundle-id uint)
  )
  (is-some (map-get? bundle-products {
    bundle-id: bundle-id,
    product-id: product-id,
  }))
)

;; Get product discount if active
(define-read-only (get-active-discount (product-id uint))
  (let ((discount-data (map-get? product-discounts { product-id: product-id })))
    (if (is-some discount-data)
      (let (
          (discount (unwrap-panic discount-data))
          (current-time (get-current-time))
        )
        (if (and
            (>= current-time (get start-time discount))
            (<= current-time (get end-time discount))
          )
          discount-data
          none
        )
      )
      none
    )
  )
)

;; Get product metrics
(define-read-only (get-product-metrics (product-id uint))
  (map-get? product-metrics { product-id: product-id })
)

;; Public functions
;; Create a new product
(define-public (create-product
    (name (string-ascii 100))
    (description (string-utf8 500))
    (price uint)
  )
  (let (
      (product-id (var-get next-product-id))
      (current-time (get-current-time))
    )
    ;; Increment the product ID counter
    (var-set next-product-id (+ product-id u1))
    ;; Store the product details
    (map-set products { product-id: product-id } {
      name: name,
      description: description,
      price: price,
      merchant: tx-sender,
      created-at: current-time,
      updated-at: current-time,
      active: true,
      featured: false,
    })
    ;; Initialize metrics for the new product
    (initialize-metrics product-id)
    ;; Return the product ID
    (ok product-id)
  )
)

;; Update an existing product
(define-public (update-product
    (product-id uint)
    (name (string-ascii 100))
    (description (string-utf8 500))
    (price uint)
    (active bool)
  )
  (let ((product-data (map-get? products { product-id: product-id })))
    (if (is-none product-data)
      ERR-PRODUCT-NOT-FOUND
      (if (not (is-product-merchant product-id))
        ERR-NOT-AUTHORIZED
        (let ((current-product (unwrap-panic product-data)))
          (map-set products { product-id: product-id } {
            name: name,
            description: description,
            price: price,
            merchant: (get merchant current-product),
            created-at: (get created-at current-product),
            updated-at: (get-current-time),
            active: active,
            featured: (get featured current-product),
          })
          (ok true)
        )
      )
    )
  )
)

;; Set product featured status
(define-public (set-product-featured
    (product-id uint)
    (featured bool)
  )
  (let ((product-data (map-get? products { product-id: product-id })))
    (if (is-none product-data)
      ERR-PRODUCT-NOT-FOUND
      (if (not (is-product-merchant product-id))
        ERR-NOT-AUTHORIZED
        (let ((current-product (unwrap-panic product-data)))
          (map-set products { product-id: product-id } {
            name: (get name current-product),
            description: (get description current-product),
            price: (get price current-product),
            merchant: (get merchant current-product),
            created-at: (get created-at current-product),
            updated-at: (get-current-time),
            active: (get active current-product),
            featured: featured,
          })
          (ok true)
        )
      )
    )
  )
)

;; Create a new category
(define-public (create-category
    (name (string-ascii 50))
    (description (string-utf8 200))
    (parent-category-id (optional uint))
  )
  (let (
      (category-id (var-get next-category-id))
      (current-time (get-current-time))
    )
    ;; Check if parent category exists when specified
    (if (and
        (is-some parent-category-id)
        (is-none (map-get? categories { category-id: (unwrap-panic parent-category-id) }))
      )
      ERR-CATEGORY-NOT-FOUND
      (begin
        ;; Increment the category ID counter
        (var-set next-category-id (+ category-id u1))
        ;; Store the category details
        (map-set categories { category-id: category-id } {
          name: name,
          description: description,
          parent-category-id: parent-category-id,
          created-at: current-time,
          updated-at: current-time,
        })
        ;; Return the category ID
        (ok category-id)
      )
    )
  )
)

;; Add product to category
(define-public (add-product-to-category
    (product-id uint)
    (category-id uint)
  )
  (let (
      (product-data (map-get? products { product-id: product-id }))
      (category-data (map-get? categories { category-id: category-id }))
    )
    (if (is-none product-data)
      ERR-PRODUCT-NOT-FOUND
      (if (is-none category-data)
        ERR-CATEGORY-NOT-FOUND
        (if (not (is-product-merchant product-id))
          ERR-NOT-AUTHORIZED
          (begin
            (map-set product-categories {
              product-id: product-id,
              category-id: category-id,
            } { assigned-at: (get-current-time) }
            )
            (ok true)
          )
        )
      )
    )
  )
)

;; Remove product from category
(define-public (remove-product-from-category
    (product-id uint)
    (category-id uint)
  )
  (let (
      (product-data (map-get? products { product-id: product-id }))
      (category-data (map-get? categories { category-id: category-id }))
    )
    (if (is-none product-data)
      ERR-PRODUCT-NOT-FOUND
      (if (is-none category-data)
        ERR-CATEGORY-NOT-FOUND
        (if (not (is-product-merchant product-id))
          ERR-NOT-AUTHORIZED
          (begin
            (map-delete product-categories {
              product-id: product-id,
              category-id: category-id,
            })
            (ok true)
          )
        )
      )
    )
  )
)

;; Set product discount
(define-public (set-product-discount
    (product-id uint)
    (discount-percentage uint)
    (start-time uint)
    (end-time uint)
  )
  (let (
      (product-data (map-get? products { product-id: product-id }))
      (current-time (get-current-time))
    )
    (if (is-none product-data)
      ERR-PRODUCT-NOT-FOUND
      (if (not (is-product-merchant product-id))
        ERR-NOT-AUTHORIZED
        (if (> discount-percentage u100)
          ERR-INVALID-DISCOUNT
          (if (>= start-time end-time)
            ERR-INVALID-DATES
            (begin
              (map-set product-discounts { product-id: product-id } {
                discount-percentage: discount-percentage,
                start-time: start-time,
                end-time: end-time,
                created-at: current-time,
              })
              (ok true)
            )
          )
        )
      )
    )
  )
)

;; Remove product discount
(define-public (remove-product-discount (product-id uint))
  (let ((product-data (map-get? products { product-id: product-id })))
    (if (is-none product-data)
      ERR-PRODUCT-NOT-FOUND
      (if (not (is-product-merchant product-id))
        ERR-NOT-AUTHORIZED
        (begin
          (map-delete product-discounts { product-id: product-id })
          (ok true)
        )
      )
    )
  )
)

;; Create product bundle
(define-public (create-bundle
    (name (string-ascii 100))
    (description (string-utf8 300))
    (price uint)
  )
  (let (
      (bundle-id (var-get next-bundle-id))
      (current-time (get-current-time))
    )
    ;; Increment the bundle ID counter
    (var-set next-bundle-id (+ bundle-id u1))
    ;; Store the bundle details
    (map-set bundles { bundle-id: bundle-id } {
      name: name,
      description: description,
      price: price,
      merchant: tx-sender,
      created-at: current-time,
      updated-at: current-time,
      active: true,
    })
    ;; Return the bundle ID
    (ok bundle-id)
  )
)

;; Update bundle
(define-public (update-bundle
    (bundle-id uint)
    (name (string-ascii 100))
    (description (string-utf8 300))
    (price uint)
    (active bool)
  )
  (let ((bundle-data (map-get? bundles { bundle-id: bundle-id })))
    (if (is-none bundle-data)
      ERR-BUNDLE-NOT-FOUND
      (if (not (is-bundle-merchant bundle-id))
        ERR-NOT-AUTHORIZED
        (let ((current-bundle (unwrap-panic bundle-data)))
          (map-set bundles { bundle-id: bundle-id } {
            name: name,
            description: description,
            price: price,
            merchant: (get merchant current-bundle),
            created-at: (get created-at current-bundle),
            updated-at: (get-current-time),
            active: active,
          })
          (ok true)
        )
      )
    )
  )
)

;; Add product to bundle
(define-public (add-product-to-bundle
    (bundle-id uint)
    (product-id uint)
  )
  (let (
      (bundle-data (map-get? bundles { bundle-id: bundle-id }))
      (product-data (map-get? products { product-id: product-id }))
    )
    (if (is-none bundle-data)
      ERR-BUNDLE-NOT-FOUND
      (if (is-none product-data)
        ERR-PRODUCT-NOT-FOUND
        (if (not (is-bundle-merchant bundle-id))
          ERR-NOT-AUTHORIZED
          (if (is-product-in-bundle product-id bundle-id)
            ERR-PRODUCT-ALREADY-IN-BUNDLE
            (begin
              (map-set bundle-products {
                bundle-id: bundle-id,
                product-id: product-id,
              } { added-at: (get-current-time) }
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

;; Remove product from bundle
(define-public (remove-product-from-bundle
    (bundle-id uint)
    (product-id uint)
  )
  (let ((bundle-data (map-get? bundles { bundle-id: bundle-id })))
    (if (is-none bundle-data)
      ERR-BUNDLE-NOT-FOUND
      (if (not (is-bundle-merchant bundle-id))
        ERR-NOT-AUTHORIZED
        (if (not (is-product-in-bundle product-id bundle-id))
          ERR-PRODUCT-NOT-IN-BUNDLE
          (begin
            (map-delete bundle-products {
              bundle-id: bundle-id,
              product-id: product-id,
            })
            (ok true)
          )
        )
      )
    )
  )
)

;; Record product view
(define-public (record-product-view (product-id uint))
  (let ((metrics-data (map-get? product-metrics { product-id: product-id })))
    (if (is-none metrics-data)
      ERR-PRODUCT-NOT-FOUND
      (let ((current-metrics (unwrap-panic metrics-data)))
        (map-set product-metrics { product-id: product-id } {
          view-count: (+ (get view-count current-metrics) u1),
          affiliate-sales-count: (get affiliate-sales-count current-metrics),
          direct-sales-count: (get direct-sales-count current-metrics),
          revenue: (get revenue current-metrics),
          last-updated: (get-current-time),
        })
        (ok true)
      )
    )
  )
)
