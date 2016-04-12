
(load "cluster_conf.ss")

(define msa001 (make-msa 'msa001 '1.1-24 'ATCSQ01_1))
(define msa002 (make-msa 'msa002 '1.1-24 'ATCSQ01_1))
(define msa003 (make-msa 'msa003 '1.1-24 'ATCSQ01_2))
(define msa004 (make-msa 'msa004 '1.1-24 'ATCSQ01_2))
(define msa005 (make-msa 'msa005 '1.1-24 'ATCSQ01_3))
(define msa006 (make-msa 'msa006 '1.1-24 'ATCSQ01_3))
(define msa007 (make-msa 'msa007 '1.1-24 'ATCSQ01_4))
(define msa008 (make-msa 'msa008 '1.1-24 'ATCSQ01_4))

(define n001 (make-node 'n001 8 2 'ATCSQ01_1))
(define n002 (make-node 'n002 8 2 'ATCSQ01_1))
(define n003 (make-node 'n003 8 2 'ATCSQ01_2))
(define n004 (make-node 'n004 8 2 'ATCSQ01_2))
(define n005 (make-node 'n005 8 2 'ATCSQ01_3))
(define n006 (make-node 'n006 8 2 'ATCSQ01_3))
(define n007 (make-node 'n007 8 2 'ATCSQ01_4))
(define n008 (make-node 'n008 8 2 'ATCSQ01_4))
(define n009 (make-node 'n009 8 2 'FREE))
(define n010 (make-node 'n010 8 2 'FREE))
(define n011 (make-node 'n011 8 2 'FREE))
(define n012 (make-node 'n012 8 2 'FREE))
(define n013 (make-node 'n013 8 2 'FREE))
(define n014 (make-node 'n014 8 2 'FREE))

(define atcsq01 (make-cluster
        'ATCSQ01                                  ; cluster-name
        '16.235.162.51                            ; cluster-ip
        'sliced                                   ; cluster-type
        'MSAShelves_300-300GB                     ; msa-typefile
        'BLADE                                    ; node-type
        '/opt/hp/platform/config/xml/Profile.xml  ; profile
        "Cheng-xin.Cai@hp.com"                    ; requestor
        ""                                        ; comments
        (list (make-cab 1
            (list msa001 msa002 msa003 msa004
                  msa005 msa006 msa007 msa008)
            (list n001 n002 n003 n004 n005 n006 n007
                  n008 n009 n010 n011 n012 n013 n014)))
        nil ))

(display "\nBuilding cluster with no instance\n")
(build-obj atcsq01)
(show-obj atcsq01)

(let ( (instance1 (make-instance
            'ATCSQ01_1                                  ; instance-id
            '/opt/hp/nv/instance/sqconfig_TRITON_sqdev1 ; sqconfig
            'NRAID                                      ; storage-type
            'development                                ; user-type
            'ATCSQ011                                   ; bdr-name
            32000                                       ; bdr-port
            'yes                                        ; disable-firewall
            'no                                         ; float-ip-flag
            (list n001 n002)                            ; sql-nodes
            nil                                         ; conn-nodes
            nil                                         ; spare-nodes
            2                                           ; no-of-ase
            8                                           ; no-of-tse
            (list n001)                                 ; tlog-nodes
            (list n002)                                 ; system-nodes
            (list msa001 msa002)                        ; msa-list
            (list (make-user 'sqdev1  'seaquest 'redhat06 "0750"
                              empty nil nil nil nil))
            "Cheng-xin.Cai@hp.com"                      ; requestor
            "no"                                        ; comments
       ))
       (instance2 (make-instance
            'ATCSQ01_2                                  ; instance-id
            '/opt/hp/nv/instance/sqconfig_TRITON_sqdev2 ; sqconfig
            'NRAID                                      ; storage-type
            'development                                ; user-type
            'ATCSQ012                                   ; bdr-name
            32000                                       ; bdr-port
            'yes                                        ; disable-firewall
            'no                                         ; float-ip-flag
            (list n003 n004)                            ; sql-nodes
            nil                                         ; conn-nodes
            nil                                         ; spare-nodes
            2                                           ; no-of-ase
            8                                           ; no-of-tse
            (list n003)                                 ; tlog-nodes
            (list n004)                                 ; system-nodes
            (list msa003 msa004)                        ; msa-list
            (list (make-user 'sqdev2  'seaquest 'redhat06 "0750"
                             empty nil nil nil nil))
            "Cheng-xin.Cai@hp.com"                      ; requestor
            "no"                                        ; comments
       ))
       (instance3 (make-instance
            'ATCSQ01_3                                  ; instance-id
            '/opt/hp/nv/instance/sqconfig_TRITON_sqdev3 ; sqconfig
            'NRAID                                      ; storage-type
            'development                                ; user-type
            'ATCSQ013                                   ; bdr-name
            32000                                       ; bdr-port
            'yes                                        ; disable-firewall
            'no                                         ; float-ip-flag
            (list n005 n006)                            ; sql-nodes
            nil                                         ; conn-nodes
            nil                                         ; spare-nodes
            2                                           ; no-of-ase
            8                                           ; no-of-tse
            (list n005)                                 ; tlog-nodes
            (list n006)                                 ; system-nodes
            (list msa005 msa006)                        ; msa-list
            (list (make-user 'sqdev3  'seaquest 'redhat06 "0750"
                              empty nil nil nil nil))
            "Cheng-xin.Cai@hp.com"                      ; requestor
            "no"                                        ; comments
       ))
       (instance4 (make-instance
            'ATCSQ01_4                                  ; instance-id
            '/opt/hp/nv/instance/sqconfig_TRITON_sqdev4 ; sqconfig
            'NRAID                                      ; storage-type
            'production                                 ; user-type
            'ATCSQ014                                   ; bdr-name
            32000                                       ; bdr-port
            'yes                                        ; disable-firewall
            'no                                         ; float-ip-flag
            (list n007 n008)                            ; sql-nodes
            nil                                         ; conn-nodes
            nil                                         ; spare-nodes
            2                                           ; no-of-ase
            8                                           ; no-of-tse
            (list n007)                                 ; tlog-nodes
            (list n008)                                 ; system-nodes
            (list msa007 msa008)                        ; msa-list
            (list (make-user 'sqdev4  'seaquest 'redhat06 "0750"
                             empty nil nil nil nil))
            "Cheng-xin.Cai@hp.com"                      ; requestor
            "no"                                        ; comments
       )) )
    (display "\nAdding instance ATCSQ01_1, ATCSQ01_2, ATCSQ01_3 and ATCSQ01_4\n")
    ((atcsq01 'add-instance) instance1)
    ((atcsq01 'add-instance) instance2)
    ((atcsq01 'add-instance) instance3)
    ((atcsq01 'add-instance) instance4)
    (show-obj atcsq01)
)


(exit)

