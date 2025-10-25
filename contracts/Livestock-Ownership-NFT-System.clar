(define-non-fungible-token livestock-token uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-exists (err u102))
(define-constant err-invalid-token (err u103))

(define-map token-metadata
    uint 
    {
        breed: (string-ascii 30),
        birth-date: uint,
        health-status: (string-ascii 20),
        last-vet-check: uint,
        location: (string-ascii 50)
    }
)

(define-map token-loans
    uint
    {
        loan-amount: uint,
        lender: principal,
        due-date: uint,
        is-active: bool
    }
)

(define-data-var last-token-id uint u0)

(define-public (mint 
    (breed (string-ascii 30))
    (birth-date uint)
    (health-status (string-ascii 20))
    (location (string-ascii 50)))
    (let
        ((token-id (+ (var-get last-token-id) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (try! (nft-mint? livestock-token token-id tx-sender))
        (var-set last-token-id token-id)
        (map-set token-metadata token-id {
            breed: breed,
            birth-date: birth-date,
            health-status: health-status,
            last-vet-check: stacks-block-height,
            location: location
        })
        (ok token-id)))

(define-public (transfer 
    (token-id uint)
    (sender principal)
    (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (nft-transfer? livestock-token token-id sender recipient)))

(define-public (update-health-status
    (token-id uint)
    (new-status (string-ascii 20)))
    (let ((owner (unwrap! (nft-get-owner? livestock-token token-id) err-invalid-token))
            (metadata (unwrap! (map-get? token-metadata token-id) err-invalid-token)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set token-metadata token-id 
            (merge metadata { health-status: new-status, last-vet-check: stacks-block-height })))))

(define-public (create-loan
    (token-id uint)
    (amount uint)
    (lender principal)
    (duration uint))
    (let ((owner (unwrap! (nft-get-owner? livestock-token token-id) err-invalid-token)))
        (asserts! (is-eq tx-sender owner) err-not-token-owner)
        (ok (map-set token-loans token-id {
            loan-amount: amount,
            lender: lender,
            due-date: (+ stacks-block-height duration),
            is-active: true
        }))))

(define-public (repay-loan (token-id uint))
    (let ((loan (unwrap! (map-get? token-loans token-id) err-invalid-token))
          (owner (unwrap! (nft-get-owner? livestock-token token-id) err-invalid-token)))
        (asserts! (is-eq tx-sender owner) err-not-token-owner)
        (ok (map-set token-loans token-id 
            (merge loan { is-active: false })))))

(define-read-only (get-token-metadata (token-id uint))
    (map-get? token-metadata token-id))

(define-read-only (get-loan-details (token-id uint))
    (map-get? token-loans token-id))

(define-read-only (get-token-uri (token-id uint))
    (ok none))

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id)))

(define-read-only (get-owner (token-id uint))
    (nft-get-owner? livestock-token token-id))

(define-map breeding-records
    uint
    {
        sire-id: (optional uint),
        dam-id: (optional uint),
        breeding-date: uint,
        generation: uint
    }
)

(define-map breeding-cooldown
    uint
    {
        last-breeding: uint,
        cooldown-period: uint
    }
)

(define-constant err-breeding-cooldown (err u104))
(define-constant err-same-gender (err u105))
(define-constant err-invalid-parents (err u106))

(define-public (breed-livestock
    (sire-id uint)
    (dam-id uint)
    (offspring-breed (string-ascii 30))
    (offspring-location (string-ascii 50)))
    (let
        ((token-id (+ (var-get last-token-id) u1))
         (sire-owner (unwrap! (nft-get-owner? livestock-token sire-id) err-invalid-token))
         (dam-owner (unwrap! (nft-get-owner? livestock-token dam-id) err-invalid-token))
         (sire-metadata (unwrap! (map-get? token-metadata sire-id) err-invalid-token))
         (dam-metadata (unwrap! (map-get? token-metadata dam-id) err-invalid-token))
         (sire-breeding (default-to {generation: u0, sire-id: none, dam-id: none, breeding-date: u0} (map-get? breeding-records sire-id)))
         (dam-breeding (default-to {generation: u0, sire-id: none, dam-id: none, breeding-date: u0} (map-get? breeding-records dam-id)))
         (new-generation (+ u1 (if (> (get generation sire-breeding) (get generation dam-breeding)) 
                                   (get generation sire-breeding) 
                                   (get generation dam-breeding)))))
        (asserts! (or (is-eq tx-sender sire-owner) (is-eq tx-sender dam-owner)) err-not-token-owner)
        (asserts! (is-breeding-allowed sire-id) err-breeding-cooldown)
        (asserts! (is-breeding-allowed dam-id) err-breeding-cooldown)
        (try! (nft-mint? livestock-token token-id tx-sender))
        (var-set last-token-id token-id)
        (map-set token-metadata token-id {
            breed: offspring-breed,
            birth-date: stacks-block-height,
            health-status: "healthy",
            last-vet-check: stacks-block-height,
            location: offspring-location
        })
        (map-set breeding-records token-id {
            sire-id: (some sire-id),
            dam-id: (some dam-id),
            breeding-date: stacks-block-height,
            generation: new-generation
        })
        (map-set breeding-cooldown sire-id {
            last-breeding: stacks-block-height,
            cooldown-period: u144
        })
        (map-set breeding-cooldown dam-id {
            last-breeding: stacks-block-height,
            cooldown-period: u144
        })
        (ok token-id)))

(define-private (is-breeding-allowed (token-id uint))
    (match (map-get? breeding-cooldown token-id)
        cooldown-data (> stacks-block-height (+ (get last-breeding cooldown-data) (get cooldown-period cooldown-data)))
        true))

(define-read-only (get-breeding-record (token-id uint))
    (map-get? breeding-records token-id))

(define-read-only (get-offspring (parent-id uint))
    (filter is-offspring-of-parent (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))

(define-private (is-offspring-of-parent (token-id uint))
    (match (map-get? breeding-records token-id)
        breeding-data (or (is-eq (get sire-id breeding-data) (some token-id))
                         (is-eq (get dam-id breeding-data) (some token-id)))
        false))

(define-read-only (get-generation (token-id uint))
    (match (map-get? breeding-records token-id)
        breeding-data (get generation breeding-data)
        u0))

        (define-map insurance-policies
    uint
    {
        policy-holder: principal,
        premium-paid: uint,
        coverage-amount: uint,
        policy-start: uint,
        policy-end: uint,
        is-active: bool
    }
)

(define-map insurance-claims
    uint
    {
        claim-amount: uint,
        claim-reason: (string-ascii 50),
        claim-date: uint,
        is-approved: bool,
        is-paid: bool
    }
)

(define-map insurance-pool
    principal
    uint
)

(define-data-var total-insurance-pool uint u0)

(define-constant err-insufficient-premium (err u107))
(define-constant err-policy-expired (err u108))
(define-constant err-claim-exists (err u109))
(define-constant err-insufficient-pool (err u110))

(define-public (purchase-insurance
    (token-id uint)
    (coverage-amount uint)
    (duration uint))
    (let
        ((owner (unwrap! (nft-get-owner? livestock-token token-id) err-invalid-token))
         (premium (calculate-premium coverage-amount duration token-id))
         (policy-end (+ stacks-block-height duration)))
        (asserts! (is-eq tx-sender owner) err-not-token-owner)
        (asserts! (>= (stx-get-balance tx-sender) premium) err-insufficient-premium)
        (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
        (var-set total-insurance-pool (+ (var-get total-insurance-pool) premium))
        (map-set insurance-policies token-id {
            policy-holder: tx-sender,
            premium-paid: premium,
            coverage-amount: coverage-amount,
            policy-start: stacks-block-height,
            policy-end: policy-end,
            is-active: true
        })
        (ok true)))

(define-public (file-insurance-claim
    (token-id uint)
    (claim-amount uint)
    (reason (string-ascii 50)))
    (let
        ((owner (unwrap! (nft-get-owner? livestock-token token-id) err-invalid-token))
         (policy (unwrap! (map-get? insurance-policies token-id) err-invalid-token)))
        (asserts! (is-eq tx-sender owner) err-not-token-owner)
        (asserts! (get is-active policy) err-invalid-token)
        (asserts! (< stacks-block-height (get policy-end policy)) err-policy-expired)
        (asserts! (<= claim-amount (get coverage-amount policy)) err-insufficient-premium)
        (asserts! (is-none (map-get? insurance-claims token-id)) err-claim-exists)
        (map-set insurance-claims token-id {
            claim-amount: claim-amount,
            claim-reason: reason,
            claim-date: stacks-block-height,
            is-approved: false,
            is-paid: false
        })
        (ok true)))

(define-public (approve-claim (token-id uint))
    (let
        ((claim (unwrap! (map-get? insurance-claims token-id) err-invalid-token))
         (policy (unwrap! (map-get? insurance-policies token-id) err-invalid-token)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (>= (var-get total-insurance-pool) (get claim-amount claim)) err-insufficient-pool)
        (try! (as-contract (stx-transfer? (get claim-amount claim) tx-sender (get policy-holder policy))))
        (var-set total-insurance-pool (- (var-get total-insurance-pool) (get claim-amount claim)))
        (map-set insurance-claims token-id
            (merge claim { is-approved: true, is-paid: true }))
        (map-set insurance-policies token-id
            (merge policy { is-active: false }))
        (ok true)))

(define-private (calculate-premium (coverage-amount uint) (duration uint) (token-id uint))
    (let
        ((base-rate u10)
         (metadata (unwrap-panic (map-get? token-metadata token-id)))
         (age-factor (- stacks-block-height (get birth-date metadata)))
         (risk-multiplier (if (is-eq (get health-status metadata) "healthy") u1 u2)))
        (/ (* (* coverage-amount base-rate) risk-multiplier duration) u10000)))

(define-public (contribute-to-pool (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-insurance-pool (+ (var-get total-insurance-pool) amount))
        (map-set insurance-pool tx-sender 
            (+ (default-to u0 (map-get? insurance-pool tx-sender)) amount))
        (ok true)))

(define-read-only (get-insurance-policy (token-id uint))
    (map-get? insurance-policies token-id))

(define-read-only (get-insurance-claim (token-id uint))
    (map-get? insurance-claims token-id))

(define-read-only (get-insurance-pool-balance)
    (var-get total-insurance-pool))

(define-read-only (calculate-premium-quote (coverage-amount uint) (duration uint) (token-id uint))
    (ok (calculate-premium coverage-amount duration token-id)))

(define-map marketplace-listings
    uint
    {
        seller: principal,
        price: uint,
        listed-at: uint,
        expiry: uint,
        is-active: bool
    }
)

(define-map marketplace-offers
    { token-id: uint, buyer: principal }
    {
        offer-amount: uint,
        offer-expiry: uint,
        is-active: bool
    }
)

(define-map escrow-deposits
    { token-id: uint, buyer: principal }
    uint
)

(define-data-var marketplace-fee-rate uint u250)
(define-data-var marketplace-fee-recipient principal contract-owner)

(define-constant err-not-listed (err u111))
(define-constant err-listing-expired (err u112))
(define-constant err-insufficient-payment (err u113))
(define-constant err-offer-expired (err u114))
(define-constant err-self-purchase (err u115))

(define-public (list-for-sale
    (token-id uint)
    (price uint)
    (duration uint))
    (let ((owner (unwrap! (nft-get-owner? livestock-token token-id) err-invalid-token)))
        (asserts! (is-eq tx-sender owner) err-not-token-owner)
        (map-set marketplace-listings token-id {
            seller: tx-sender,
            price: price,
            listed-at: stacks-block-height,
            expiry: (+ stacks-block-height duration),
            is-active: true
        })
        (ok true)))

(define-public (delist-from-sale (token-id uint))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-not-listed)))
        (asserts! (is-eq tx-sender (get seller listing)) err-not-token-owner)
        (map-set marketplace-listings token-id
            (merge listing { is-active: false }))
        (ok true)))

(define-public (purchase-livestock (token-id uint))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-not-listed))
          (owner (unwrap! (nft-get-owner? livestock-token token-id) err-invalid-token))
          (marketplace-fee (/ (* (get price listing) (var-get marketplace-fee-rate)) u10000))
          (seller-payment (- (get price listing) marketplace-fee)))
        (asserts! (get is-active listing) err-not-listed)
        (asserts! (< stacks-block-height (get expiry listing)) err-listing-expired)
        (asserts! (not (is-eq tx-sender (get seller listing))) err-self-purchase)
        (asserts! (>= (stx-get-balance tx-sender) (get price listing)) err-insufficient-payment)
        (try! (stx-transfer? seller-payment tx-sender (get seller listing)))
        (try! (stx-transfer? marketplace-fee tx-sender (var-get marketplace-fee-recipient)))
        (try! (nft-transfer? livestock-token token-id owner tx-sender))
        (map-set marketplace-listings token-id
            (merge listing { is-active: false }))
        (ok true)))

(define-public (make-offer
    (token-id uint)
    (offer-amount uint)
    (offer-duration uint))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-not-listed)))
        (asserts! (get is-active listing) err-not-listed)
        (asserts! (not (is-eq tx-sender (get seller listing))) err-self-purchase)
        (asserts! (>= (stx-get-balance tx-sender) offer-amount) err-insufficient-payment)
        (try! (stx-transfer? offer-amount tx-sender (as-contract tx-sender)))
        (map-set escrow-deposits { token-id: token-id, buyer: tx-sender } offer-amount)
        (map-set marketplace-offers { token-id: token-id, buyer: tx-sender } {
            offer-amount: offer-amount,
            offer-expiry: (+ stacks-block-height offer-duration),
            is-active: true
        })
        (ok true)))

(define-public (accept-offer
    (token-id uint)
    (buyer principal))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-not-listed))
          (offer (unwrap! (map-get? marketplace-offers { token-id: token-id, buyer: buyer }) err-invalid-token))
          (escrow-amount (unwrap! (map-get? escrow-deposits { token-id: token-id, buyer: buyer }) err-invalid-token))
          (owner (unwrap! (nft-get-owner? livestock-token token-id) err-invalid-token))
          (marketplace-fee (/ (* (get offer-amount offer) (var-get marketplace-fee-rate)) u10000))
          (seller-payment (- (get offer-amount offer) marketplace-fee)))
        (asserts! (is-eq tx-sender (get seller listing)) err-not-token-owner)
        (asserts! (get is-active offer) err-invalid-token)
        (asserts! (< stacks-block-height (get offer-expiry offer)) err-offer-expired)
        (try! (as-contract (stx-transfer? seller-payment tx-sender (get seller listing))))
        (try! (as-contract (stx-transfer? marketplace-fee tx-sender (var-get marketplace-fee-recipient))))
        (try! (nft-transfer? livestock-token token-id owner buyer))
        (map-delete escrow-deposits { token-id: token-id, buyer: buyer })
        (map-set marketplace-offers { token-id: token-id, buyer: buyer }
            (merge offer { is-active: false }))
        (map-set marketplace-listings token-id
            (merge listing { is-active: false }))
        (ok true)))

(define-public (withdraw-offer
    (token-id uint))
    (let ((offer (unwrap! (map-get? marketplace-offers { token-id: token-id, buyer: tx-sender }) err-invalid-token))
          (escrow-amount (unwrap! (map-get? escrow-deposits { token-id: token-id, buyer: tx-sender }) err-invalid-token)))
        (asserts! (get is-active offer) err-invalid-token)
        (try! (as-contract (stx-transfer? escrow-amount tx-sender tx-sender)))
        (map-delete escrow-deposits { token-id: token-id, buyer: tx-sender })
        (map-set marketplace-offers { token-id: token-id, buyer: tx-sender }
            (merge offer { is-active: false }))
        (ok true)))

(define-public (set-marketplace-fee (new-fee-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee-rate u1000) err-invalid-token)
        (var-set marketplace-fee-rate new-fee-rate)
        (ok true)))

(define-read-only (get-listing (token-id uint))
    (map-get? marketplace-listings token-id))

(define-read-only (get-offer (token-id uint) (buyer principal))
    (map-get? marketplace-offers { token-id: token-id, buyer: buyer }))

(define-read-only (get-escrow-amount (token-id uint) (buyer principal))
    (map-get? escrow-deposits { token-id: token-id, buyer: buyer }))

(define-read-only (get-marketplace-fee-rate)
    (var-get marketplace-fee-rate))

(define-read-only (calculate-marketplace-fee (price uint))
    (/ (* price (var-get marketplace-fee-rate)) u10000))