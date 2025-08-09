;; ===================================================
;; HOME REPAIR REFERRAL SYSTEM
;; ===================================================
;; A trusted contractor recommendation system with rating verification,
;; job completion tracking, and photo verification capabilities.
;;
;; Features:
;; - Contractor registration and profile management
;; - Job posting and assignment system
;; - Rating and review system with verification
;; - Photo verification for job completion
;; - Reputation scoring and trust metrics

;; ===================================================
;; CONSTANTS & ERROR CODES
;; ===================================================

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-INPUT (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-JOB-NOT-AVAILABLE (err u105))
(define-constant ERR-JOB-ALREADY-ASSIGNED (err u106))
(define-constant ERR-JOB-NOT-COMPLETED (err u107))
(define-constant ERR-ALREADY-RATED (err u108))
(define-constant ERR-INVALID-RATING (err u109))
(define-constant ERR-VERIFICATION-FAILED (err u110))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Job statuses
(define-constant JOB-STATUS-OPEN u1)
(define-constant JOB-STATUS-ASSIGNED u2)
(define-constant JOB-STATUS-COMPLETED u3)
(define-constant JOB-STATUS-VERIFIED u4)
(define-constant JOB-STATUS-DISPUTED u5)

;; Rating constants
(define-constant MIN-RATING u1)
(define-constant MAX-RATING u5)

;; Minimum stake for contractors (in microSTX)
(define-constant MIN-CONTRACTOR-STAKE u1000000) ;; 1 STX

;; ===================================================
;; DATA VARIABLES
;; ===================================================

;; Global counters
(define-data-var next-contractor-id uint u1)
(define-data-var next-job-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% (250/10000)

;; ===================================================
;; DATA MAPS
;; ===================================================

;; Contractor profiles
(define-map contractors
    { contractor-id: uint }
    {
        owner: principal,
        name: (string-ascii 50),
        specialty: (string-ascii 30),
        description: (string-ascii 200),
        contact-info: (string-ascii 100),
        license-number: (optional (string-ascii 20)),
        insurance-verified: bool,
        stake-amount: uint,
        registration-block: uint,
        total-jobs: uint,
        completed-jobs: uint,
        total-rating-points: uint,
        rating-count: uint,
        is-active: bool
    }
)

;; Contractor lookup by principal
(define-map contractor-principals
    { owner: principal }
    { contractor-id: uint }
)

;; Job listings
(define-map jobs
    { job-id: uint }
    {
        client: principal,
        title: (string-ascii 50),
        description: (string-ascii 300),
        category: (string-ascii 20),
        budget: uint,
        location: (string-ascii 100),
        status: uint,
        assigned-contractor: (optional uint),
        created-block: uint,
        assigned-block: (optional uint),
        completed-block: (optional uint),
        photo-hash: (optional (buff 32)),
        dispute-reason: (optional (string-ascii 200))
    }
)

;; Job ratings and reviews
(define-map job-ratings
    { job-id: uint }
    {
        client-rating: (optional uint),
        contractor-rating: (optional uint),
        client-review: (optional (string-ascii 200)),
        contractor-review: (optional (string-ascii 200)),
        rating-block: (optional uint)
    }
)

;; Contractor specialties tracking
(define-map specialty-contractors
    { specialty: (string-ascii 30), contractor-id: uint }
    { is-listed: bool }
)

;; Platform earnings
(define-map platform-earnings
    { block-height: uint }
    { total-earned: uint }
)

;; ===================================================
;; PRIVATE FUNCTIONS
;; ===================================================

;; Calculate average rating for a contractor
(define-private (calculate-average-rating (total-points uint) (count uint))
    (if (> count u0)
        (/ (* total-points u100) count) ;; Return as percentage (e.g., 450 = 4.5 stars)
        u0
    )
)

;; Validate rating value
(define-private (is-valid-rating (rating uint))
    (and (>= rating MIN-RATING) (<= rating MAX-RATING))
)

;; Calculate platform fee
(define-private (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-rate)) u10000)
)

;; ===================================================
;; PUBLIC FUNCTIONS - CONTRACTOR MANAGEMENT
;; ===================================================

;; Register as a contractor
(define-public (register-contractor
    (name (string-ascii 50))
    (specialty (string-ascii 30))
    (description (string-ascii 200))
    (contact-info (string-ascii 100))
    (license-number (optional (string-ascii 20)))
    (insurance-verified bool)
)
    (let (
        (contractor-id (var-get next-contractor-id))
        (stake-amount (if insurance-verified MIN-CONTRACTOR-STAKE (* MIN-CONTRACTOR-STAKE u2)))
    )
        ;; Check if contractor already exists
        (asserts! (is-none (map-get? contractor-principals { owner: tx-sender })) ERR-ALREADY-EXISTS)

        ;; Validate input
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (> (len specialty) u0) ERR-INVALID-INPUT)

        ;; Create contractor profile
        (map-set contractors
            { contractor-id: contractor-id }
            {
                owner: tx-sender,
                name: name,
                specialty: specialty,
                description: description,
                contact-info: contact-info,
                license-number: license-number,
                insurance-verified: insurance-verified,
                stake-amount: stake-amount,
                registration-block: stacks-block-height,
                total-jobs: u0,
                completed-jobs: u0,
                total-rating-points: u0,
                rating-count: u0,
                is-active: true
            }
        )

        ;; Create principal lookup
        (map-set contractor-principals
            { owner: tx-sender }
            { contractor-id: contractor-id }
        )

        ;; Add to specialty index
        (map-set specialty-contractors
            { specialty: specialty, contractor-id: contractor-id }
            { is-listed: true }
        )

        ;; Update counter
        (var-set next-contractor-id (+ contractor-id u1))

        ;; Transfer stake (simplified - in production would handle STX transfer)
        (ok contractor-id)
    )
)

;; Update contractor profile
(define-public (update-contractor-profile
    (description (string-ascii 200))
    (contact-info (string-ascii 100))
    (license-number (optional (string-ascii 20)))
)
    (let (
        (contractor-lookup (unwrap! (map-get? contractor-principals { owner: tx-sender }) ERR-NOT-FOUND))
        (contractor-id (get contractor-id contractor-lookup))
        (contractor (unwrap! (map-get? contractors { contractor-id: contractor-id }) ERR-NOT-FOUND))
    )
        ;; Verify ownership
        (asserts! (is-eq (get owner contractor) tx-sender) ERR-UNAUTHORIZED)

        ;; Update contractor profile
        (map-set contractors
            { contractor-id: contractor-id }
            (merge contractor {
                description: description,
                contact-info: contact-info,
                license-number: license-number
            })
        )
        (ok true)
    )
)

;; Deactivate contractor account
(define-public (deactivate-contractor)
    (let (
        (contractor-lookup (unwrap! (map-get? contractor-principals { owner: tx-sender }) ERR-NOT-FOUND))
        (contractor-id (get contractor-id contractor-lookup))
        (contractor (unwrap! (map-get? contractors { contractor-id: contractor-id }) ERR-NOT-FOUND))
    )
        ;; Verify ownership
        (asserts! (is-eq (get owner contractor) tx-sender) ERR-UNAUTHORIZED)

        ;; Deactivate contractor
        (map-set contractors
            { contractor-id: contractor-id }
            (merge contractor { is-active: false })
        )
        (ok true)
    )
)

;; ===================================================
;; PUBLIC FUNCTIONS - JOB MANAGEMENT
;; ===================================================

;; Post a new job
(define-public (post-job
    (title (string-ascii 50))
    (description (string-ascii 300))
    (category (string-ascii 20))
    (budget uint)
    (location (string-ascii 100))
)
    (let ((job-id (var-get next-job-id)))
        ;; Validate input
        (asserts! (> (len title) u0) ERR-INVALID-INPUT)
        (asserts! (> budget u0) ERR-INVALID-INPUT)

        ;; Create job listing
        (map-set jobs
            { job-id: job-id }
            {
                client: tx-sender,
                title: title,
                description: description,
                category: category,
                budget: budget,
                location: location,
                status: JOB-STATUS-OPEN,
                assigned-contractor: none,
                created-block: stacks-block-height,
                assigned-block: none,
                completed-block: none,
                photo-hash: none,
                dispute-reason: none
            }
        )

        ;; Initialize empty ratings
        (map-set job-ratings
            { job-id: job-id }
            {
                client-rating: none,
                contractor-rating: none,
                client-review: none,
                contractor-review: none,
                rating-block: none
            }
        )

        ;; Update counter
        (var-set next-job-id (+ job-id u1))
        (ok job-id)
    )
)

;; Contractor applies for job
(define-public (apply-for-job (job-id uint))
    (let (
        (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-NOT-FOUND))
        (contractor-lookup (unwrap! (map-get? contractor-principals { owner: tx-sender }) ERR-NOT-FOUND))
        (contractor-id (get contractor-id contractor-lookup))
        (contractor (unwrap! (map-get? contractors { contractor-id: contractor-id }) ERR-NOT-FOUND))
    )
        ;; Verify contractor is active
        (asserts! (get is-active contractor) ERR-UNAUTHORIZED)

        ;; Verify job is open
        (asserts! (is-eq (get status job) JOB-STATUS-OPEN) ERR-JOB-NOT-AVAILABLE)

        ;; Assign job to contractor
        (map-set jobs
            { job-id: job-id }
            (merge job {
                status: JOB-STATUS-ASSIGNED,
                assigned-contractor: (some contractor-id),
                assigned-block: (some stacks-block-height)
            })
        )

        ;; Update contractor stats
        (map-set contractors
            { contractor-id: contractor-id }
            (merge contractor {
                total-jobs: (+ (get total-jobs contractor) u1)
            })
        )

        (ok true)
    )
)

;; Mark job as completed with photo verification
(define-public (complete-job (job-id uint) (photo-hash (buff 32)))
    (let (
        (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-NOT-FOUND))
        (contractor-lookup (unwrap! (map-get? contractor-principals { owner: tx-sender }) ERR-NOT-FOUND))
        (contractor-id (get contractor-id contractor-lookup))
        (assigned-contractor-id (unwrap! (get assigned-contractor job) ERR-UNAUTHORIZED))
    )
        ;; Verify contractor ownership
        (asserts! (is-eq contractor-id assigned-contractor-id) ERR-UNAUTHORIZED)

        ;; Verify job is assigned
        (asserts! (is-eq (get status job) JOB-STATUS-ASSIGNED) ERR-JOB-NOT-AVAILABLE)

        ;; Mark job as completed
        (map-set jobs
            { job-id: job-id }
            (merge job {
                status: JOB-STATUS-COMPLETED,
                completed-block: (some stacks-block-height),
                photo-hash: (some photo-hash)
            })
        )

        (ok true)
    )
)

;; Client verifies job completion
(define-public (verify-job-completion (job-id uint))
    (let (
        (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-NOT-FOUND))
        (contractor-id (unwrap! (get assigned-contractor job) ERR-NOT-FOUND))
        (contractor (unwrap! (map-get? contractors { contractor-id: contractor-id }) ERR-NOT-FOUND))
    )
        ;; Verify client ownership
        (asserts! (is-eq (get client job) tx-sender) ERR-UNAUTHORIZED)

        ;; Verify job is completed
        (asserts! (is-eq (get status job) JOB-STATUS-COMPLETED) ERR-JOB-NOT-COMPLETED)

        ;; Mark job as verified
        (map-set jobs
            { job-id: job-id }
            (merge job { status: JOB-STATUS-VERIFIED })
        )

        ;; Update contractor completed jobs count
        (map-set contractors
            { contractor-id: contractor-id }
            (merge contractor {
                completed-jobs: (+ (get completed-jobs contractor) u1)
            })
        )

        ;; Calculate and transfer payment (simplified)
        (let (
            (budget (get budget job))
            (platform-fee (calculate-platform-fee budget))
            (contractor-payment (- budget platform-fee))
        )
            ;; In production, handle actual STX transfers here
            (ok true)
        )
    )
)

;; ===================================================
;; PUBLIC FUNCTIONS - RATING SYSTEM
;; ===================================================

;; Rate a contractor after job completion
(define-public (rate-contractor
    (job-id uint)
    (rating uint)
    (review (optional (string-ascii 200)))
)
    (let (
        (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-NOT-FOUND))
        (job-rating (unwrap! (map-get? job-ratings { job-id: job-id }) ERR-NOT-FOUND))
        (contractor-id (unwrap! (get assigned-contractor job) ERR-NOT-FOUND))
        (contractor (unwrap! (map-get? contractors { contractor-id: contractor-id }) ERR-NOT-FOUND))
    )
        ;; Verify client ownership
        (asserts! (is-eq (get client job) tx-sender) ERR-UNAUTHORIZED)

        ;; Verify job is verified
        (asserts! (is-eq (get status job) JOB-STATUS-VERIFIED) ERR-VERIFICATION-FAILED)

        ;; Verify rating hasn't been submitted
        (asserts! (is-none (get client-rating job-rating)) ERR-ALREADY-RATED)

        ;; Validate rating
        (asserts! (is-valid-rating rating) ERR-INVALID-RATING)

        ;; Update job rating
        (map-set job-ratings
            { job-id: job-id }
            (merge job-rating {
                client-rating: (some rating),
                client-review: review,
                rating-block: (some stacks-block-height)
            })
        )

        ;; Update contractor rating stats
        (map-set contractors
            { contractor-id: contractor-id }
            (merge contractor {
                total-rating-points: (+ (get total-rating-points contractor) rating),
                rating-count: (+ (get rating-count contractor) u1)
            })
        )

        (ok true)
    )
)

;; Rate a client after job completion
(define-public (rate-client
    (job-id uint)
    (rating uint)
    (review (optional (string-ascii 200)))
)
    (let (
        (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-NOT-FOUND))
        (job-rating (unwrap! (map-get? job-ratings { job-id: job-id }) ERR-NOT-FOUND))
        (contractor-lookup (unwrap! (map-get? contractor-principals { owner: tx-sender }) ERR-NOT-FOUND))
        (contractor-id (get contractor-id contractor-lookup))
        (assigned-contractor-id (unwrap! (get assigned-contractor job) ERR-NOT-FOUND))
    )
        ;; Verify contractor ownership
        (asserts! (is-eq contractor-id assigned-contractor-id) ERR-UNAUTHORIZED)

        ;; Verify job is verified
        (asserts! (is-eq (get status job) JOB-STATUS-VERIFIED) ERR-VERIFICATION-FAILED)

        ;; Verify rating hasn't been submitted
        (asserts! (is-none (get contractor-rating job-rating)) ERR-ALREADY-RATED)

        ;; Validate rating
        (asserts! (is-valid-rating rating) ERR-INVALID-RATING)

        ;; Update job rating
        (map-set job-ratings
            { job-id: job-id }
            (merge job-rating {
                contractor-rating: (some rating),
                contractor-review: review,
                rating-block: (some stacks-block-height)
            })
        )

        (ok true)
    )
)

;; ===================================================
;; READ-ONLY FUNCTIONS
;; ===================================================

;; Get contractor details
(define-read-only (get-contractor (contractor-id uint))
    (map-get? contractors { contractor-id: contractor-id })
)

;; Get contractor by principal
(define-read-only (get-contractor-by-principal (owner principal))
    (match (map-get? contractor-principals { owner: owner })
        contractor-lookup (map-get? contractors { contractor-id: (get contractor-id contractor-lookup) })
        none
    )
)

;; Get contractor average rating
(define-read-only (get-contractor-rating (contractor-id uint))
    (match (map-get? contractors { contractor-id: contractor-id })
        contractor (calculate-average-rating
            (get total-rating-points contractor)
            (get rating-count contractor)
        )
        u0
    )
)

;; Get job details
(define-read-only (get-job (job-id uint))
    (map-get? jobs { job-id: job-id })
)

;; Get job rating
(define-read-only (get-job-rating (job-id uint))
    (map-get? job-ratings { job-id: job-id })
)

;; Check if contractor exists in specialty
(define-read-only (is-contractor-in-specialty (specialty (string-ascii 30)) (contractor-id uint))
    (default-to false (get is-listed (map-get? specialty-contractors { specialty: specialty, contractor-id: contractor-id })))
)

;; Get platform fee rate
(define-read-only (get-platform-fee-rate)
    (var-get platform-fee-rate)
)

;; Get next contractor ID
(define-read-only (get-next-contractor-id)
    (var-get next-contractor-id)
)

;; Get next job ID
(define-read-only (get-next-job-id)
    (var-get next-job-id)
)

;; ===================================================
;; ADMIN FUNCTIONS
;; ===================================================

;; Update platform fee rate (admin only)
(define-public (set-platform-fee-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (<= new-rate u1000) ERR-INVALID-INPUT) ;; Max 10%
        (var-set platform-fee-rate new-rate)
        (ok true)
    )
)
