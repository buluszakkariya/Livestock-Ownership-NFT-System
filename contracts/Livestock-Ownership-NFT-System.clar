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
