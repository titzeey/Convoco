;; Community Guild Voting System
;; A guild-based voting contract for community decisions and member proposals

;; Constants
(define-constant guild-master tx-sender)
(define-constant err-master-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-voting-ended (err u103))
(define-constant err-voting-not-ended (err u104))
(define-constant err-insufficient-reputation (err u105))
(define-constant err-proposal-not-active (err u106))

;; Data Variables
(define-data-var initiative-counter uint u0)
(define-data-var min-reputation uint u10) ;; Minimum reputation needed to participate
(define-data-var deliberation-period uint u1008) ;; Default 1008 blocks (~7 days)

;; Data Maps
(define-map guild-initiatives
  uint
  {
    name: (string-utf8 256),
    details: (string-utf8 1024),
    champion: principal,
    start-block: uint,
    end-block: uint,
    support-votes: uint,
    oppose-votes: uint,
    total-members: uint,
    enacted: bool,
    active: bool
  }
)

(define-map member-decisions
  { initiative-id: uint, member: principal }
  { decision: bool, reputation-weight: uint }
)

(define-map member-reputation
  principal
  uint
)

;; Read-only functions
(define-read-only (get-initiative (initiative-id uint))
  (map-get? guild-initiatives initiative-id)
)

(define-read-only (get-member-decision (initiative-id uint) (member principal))
  (map-get? member-decisions { initiative-id: initiative-id, member: member })
)

(define-read-only (get-reputation (member principal))
  (default-to u0 (map-get? member-reputation member))
)

(define-read-only (get-initiative-count)
  (var-get initiative-counter)
)

(define-read-only (is-deliberation-active (initiative-id uint))
  (match (map-get? guild-initiatives initiative-id)
    initiative (and 
      (get active initiative)
      (>= block-height (get start-block initiative))
      (<= block-height (get end-block initiative))
    )
    false
  )
)

(define-read-only (get-initiative-status (initiative-id uint))
  (match (map-get? guild-initiatives initiative-id)
    initiative (some {
      initiative-id: initiative-id,
      name: (get name initiative),
      support-votes: (get support-votes initiative),
      oppose-votes: (get oppose-votes initiative),
      total-members: (get total-members initiative),
      is-active: (is-deliberation-active initiative-id),
      enacted: (get enacted initiative)
    })
    none
  )
)

;; Public functions
(define-public (champion-initiative (name (string-utf8 256)) (details (string-utf8 1024)))
  (let
    (
      (initiative-id (+ (var-get initiative-counter) u1))
      (start-block block-height)
      (end-block (+ block-height (var-get deliberation-period)))
    )
    ;; Check if member has minimum reputation
    (asserts! (>= (get-reputation tx-sender) (var-get min-reputation)) err-insufficient-reputation)
    
    ;; Create the initiative
    (map-set guild-initiatives initiative-id {
      name: name,
      details: details,
      champion: tx-sender,
      start-block: start-block,
      end-block: end-block,
      support-votes: u0,
      oppose-votes: u0,
      total-members: u0,
      enacted: false,
      active: true
    })
    
    ;; Update initiative counter
    (var-set initiative-counter initiative-id)
    
    (print { event: "initiative-championed", initiative-id: initiative-id, champion: tx-sender })
    (ok initiative-id)
  )
)

(define-public (cast-decision (initiative-id uint) (support bool))
  (let
    (
      (member tx-sender)
      (reputation (get-reputation member))
      (initiative (unwrap! (map-get? guild-initiatives initiative-id) err-not-found))
    )
    ;; Validation checks
    (asserts! (is-deliberation-active initiative-id) err-proposal-not-active)
    (asserts! (>= reputation (var-get min-reputation)) err-insufficient-reputation)
    (asserts! (is-none (map-get? member-decisions { initiative-id: initiative-id, member: member })) err-already-voted)
    
    ;; Record the decision
    (map-set member-decisions 
      { initiative-id: initiative-id, member: member }
      { decision: support, reputation-weight: reputation }
    )
    
    ;; Update initiative vote counts
    (map-set guild-initiatives initiative-id
      (merge initiative {
        support-votes: (if support 
          (+ (get support-votes initiative) reputation)
          (get support-votes initiative)
        ),
        oppose-votes: (if support
          (get oppose-votes initiative)
          (+ (get oppose-votes initiative) reputation)
        ),
        total-members: (+ (get total-members initiative) u1)
      })
    )
    
    (print { event: "decision-cast", initiative-id: initiative-id, member: member, support: support, reputation: reputation })
    (ok true)
  )
)

(define-public (enact-initiative (initiative-id uint))
  (let
    (
      (initiative (unwrap! (map-get? guild-initiatives initiative-id) err-not-found))
    )
    ;; Check if deliberation has ended
    (asserts! (> block-height (get end-block initiative)) err-voting-not-ended)
    (asserts! (not (get enacted initiative)) err-proposal-not-active)
    
    ;; Mark as enacted
    (map-set guild-initiatives initiative-id
      (merge initiative { enacted: true })
    )
    
    (let
      (
        (support-votes (get support-votes initiative))
        (oppose-votes (get oppose-votes initiative))
        (approved (> support-votes oppose-votes))
      )
      (print { 
        event: "initiative-enacted", 
        initiative-id: initiative-id, 
        approved: approved,
        support-votes: support-votes,
        oppose-votes: oppose-votes
      })
      (ok approved)
    )
  )
)

;; Guild Master functions
(define-public (set-member-reputation (member principal) (reputation uint))
  (begin
    (asserts! (is-eq tx-sender guild-master) err-master-only)
    (map-set member-reputation member reputation)
    (print { event: "reputation-updated", member: member, reputation: reputation })
    (ok true)
  )
)

(define-public (set-min-reputation (new-min uint))
  (begin
    (asserts! (is-eq tx-sender guild-master) err-master-only)
    (var-set min-reputation new-min)
    (ok true)
  )
)

(define-public (set-deliberation-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender guild-master) err-master-only)
    (var-set deliberation-period new-period)
    (ok true)
  )
)

(define-public (deactivate-initiative (initiative-id uint))
  (let
    (
      (initiative (unwrap! (map-get? guild-initiatives initiative-id) err-not-found))
    )
    (asserts! (is-eq tx-sender guild-master) err-master-only)
    (map-set guild-initiatives initiative-id
      (merge initiative { active: false })
    )
    (print { event: "initiative-deactivated", initiative-id: initiative-id })
    (ok true)
  )
)

;; Initialize contract with guild master reputation
(map-set member-reputation guild-master u100)