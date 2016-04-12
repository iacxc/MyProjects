
(load "cluster_conf.ss")

(define msa001 (make-msa 'msa001 '1.1-24 'ATCSQ01))
(define msa002 (make-msa 'msa002 '1.1-24 'ATCSQ01))
(define msa003 (make-msa 'msa003 '1.1-24 'ATCSQ01))
(define msa004 (make-msa 'msa004 '1.1-24 'ATCSQ01))
(define msa005 (make-msa 'msa005 '1.1-24 'ATCSQ01))
(define msa006 (make-msa 'msa006 '1.1-24 'ATCSQ01))
(define msa007 (make-msa 'msa007 '1.1-24 'ATCSQ01))
(define msa008 (make-msa 'msa008 '1.1-24 'ATCSQ01))

(define n001 (make-node 'n001 8 2 'ATCSQ01))
(define n002 (make-node 'n002 8 2 'ATCSQ01))
(define n003 (make-node 'n003 8 2 'ATCSQ01))
(define n004 (make-node 'n004 8 2 'ATCSQ01))
(define n005 (make-node 'n005 8 2 'ATCSQ01))
(define n006 (make-node 'n006 8 2 'ATCSQ01))
(define n007 (make-node 'n007 8 2 'ATCSQ01))
(define n008 (make-node 'n008 8 2 'ATCSQ01))
(define n009 (make-node 'n009 8 2 'FREE))
(define n010 (make-node 'n010 8 2 'FREE))
(define n011 (make-node 'n011 8 2 'FREE))
(define n012 (make-node 'n012 8 2 'FREE))
(define n013 (make-node 'n013 8 2 'FREE))
(define n014 (make-node 'n014 8 2 'FREE))

(define atcsq01
    (let ( (squserN (make-user
                'squserN      ; user id
                'seaquest     ; group
                'redhat06     ; password
                "0750"        ; permission
                '/opt/hp/nv/instance/sqconfig_ATCSQ01_squserN  ; sqconfig
                (list n001 n002 n003 n004 n005 n006 n007 n008) ; sql-nodes
                nil                                            ; conn-nodes
                (list n001 n002 n003 n004)                     ; tlog-nodes
                (list n005 n006 n007 n008)                     ; system-nodes
                ))
           (sqdev1 (make-user 'sqdev1  'seaquest 'redhat06 "0750"
                              '/opt/hp/nv/instance/sqconfig_ATCSQ01_sqdev1
                              (list n001 n002 n003 n004 n005 n006 n007 n008)
                              nil
                              (list n001)
                              (list n002)))
         )
        (make-cluster
            'ATCSQ01                                  ; cluster-name
            '16.235.162.51                            ; cluster-ip
            'shared                                   ; cluster-type
            'MSAShelves_300-300GB                     ; msa-typefile
            'BLADE                                    ; node-type
            '/opt/hp/platform/config/xml/Profile.xml  ; profile
            "Cheng-xin.Cai@hp.com"                    ; requestor
            ""                                        ; comments
            ;; list of cabinets
            (list (make-cab 1
                (list msa001 msa002 msa003 msa004
                      msa005 msa006 msa007 msa008)
                (list n001 n002 n003 n004 n005 n006 n007
                      n008 n009 n010 n011 n012 n013 n014)))
                ;; list of instances
            (list (make-instance
                'ATCSQ01                                        ; instance-id
                '/opt/hp/nv/instance/sqconfig_TRITON_squserN    ; sqconfig
                'NRAID                                          ; storage-type
                'development                                    ; user-type
                'ATCSQ01                                        ; bdr-name
                32000                                           ; bdr-port
                'yes                                            ; disable-firewall
                'no                                             ; float-ip-flag
                (list n001 n002 n003 n004 n005 n006 n007 n008)  ; sql-nodes
                nil                                             ; conn-nodes
                nil                                             ; spare-nodes
                2                                               ; no-of-ase
                8                                               ; no-of-tse
                nil                                             ; tlog-nodes
                nil                                             ; system-nodes
                (list msa001 msa002 msa003 msa004               ; msa-list
                      msa005 msa006 msa007 msa008)
                (list squserN sqdev1)                           ; user-list
                "Cheng-xin.Cai@hp.com"                          ; requestor
                "no"                                            ; comments
                )))))

(display "\nBuilding cluster with sqdev1\n")
(build-obj atcsq01)
(show-obj atcsq01)

(let ( (sqdev2 (make-user 'sqdev2  'seaquest 'redhat06 "0750"
                          '/opt/hp/nv/instance/sqconfig_ATCSQ01_sqdev1
                          (list n001 n002 n003 n004)
                          nil
                          (list n001)
                          (list n002)))
       (sqdev3 (make-user 'sqdev3  'seaquest 'redhat06 "0750"
                          '/opt/hp/nv/instance/sqconfig_ATCSQ01_sqdev1
                          (list  n005 n006 n007 n008)
                          nil
                          (list n005)
                          (list n006)))
       (sqdev4 (make-user 'sqdev4  'seaquest 'redhat06 "0750"
                          '/opt/hp/nv/instance/sqconfig_ATCSQ01_sqdev4
                          (list n003 n004 n005 n006)
                          nil
                          (list n003)
                          (list n004))) )

    (display "\nAdding sqdev2, sqdev3 and sqdev4\n")
    ((atcsq01 'add-user) sqdev2)
    ((atcsq01 'add-user) sqdev3)
    ((atcsq01 'add-user) sqdev4)
    (show-obj atcsq01)
)

(display "\nDelete sqdev2 and sqdev3\n")
((atcsq01 'del-user) 'sqdev2)
((atcsq01 'del-user) 'sqdev3)
(show-obj atcsq01)

;(exit)
