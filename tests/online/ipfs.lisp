;; https://ipfs.io/ipfs/Qme7ss3ARVgxv6rXqVPiikMJ8u2NLgmgszg13pYrDKEoiu
(defvar *url-image* "Qme7ss3ARVgxv6rXqVPiikMJ8u2NLgmgszg13pYrDKEoiu")

;; TODO: Need path with subpath.
;; ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/wiki/Vincent_van_Gogh.html
;; ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/I/m/Van_Gogh_Museum_Amsterdam.jpg
;; ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/I/m/Van_Gogh_-_Bildnis_der_Mutter_des_K%C3%BCnstlers.jpeg

;; http://127.0.0.1:8080/ipfs/Qme7ss3ARVgxv6rXqVPiikMJ8u2NLgmgszg13pYrDKEoiu

;; REFERENCE:
;; https://www.reddit.com/r/ipfs/comments/63ev6h/list_of_ipfs_websites/
;; https://github.com/brave/brave-browser/issues/10220
;; https://github.com/ipfs/in-web-browsers
;; RPC: https://docs.ipfs.tech/reference/kubo/rpc/#getting-started

;; REVIEW: Website:
;; http://127.0.0.1:8080/ipfs/QmfQiLpdBDbSkb2oySwFHzNucvLkHmGFxgK4oA2BUSwi4t/
;; http://127.0.0.1:8080/ipfs/QmZBuTfLH1LLi4JqgutzBdwSYS5ybrkztnyWAfRBP729WB/archives/index.html
;; https://blog.ipfs.tech/24-uncensorable-wikipedia/

;; GATEWAYS:
;; https://ipfs.github.io/public-gateway-checker/
;; https://bafybeigdyrzt5sfp7udm7hu76uh7y26nf3efuylqabf3oclgtqy55fbzdi.ipfs.dweb.link/

;; https://github.com/ipfs/in-web-browsers


;; ADDRESS validator
;; https://github.com/ipfs-shipyard/is-ipfs


;; (CL-IPFS-API2::IPFS-CALL "cat" (("arg" "bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/I/m/Van_Gogh_-_Bildnis_der_Mutter_des_K%C3%BCnstlers.jpeg") NIL NIL) :PARAMETERS NIL :WANT-STREAM NIL)
;;       Locals:
;;         ARGUMENTS = (("arg" "bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/I/m/Van_Gogh_-_Bildnis_der_Mutter_des_K%C3%BCnstlers.jpeg") NIL NIL)
;;         CALL = "cat"
;;         METHOD = :POST
;;         PARAMETERS = NIL
;;         WANT-STREAM = NIL

;; (DRAKMA:HTTP-REQUEST "http://127.0.0.1:5001/api/v0/cat?arg=bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/I/m/Van_Gogh_-_Bildnis_der_Mutter_des_K%C3%BCnstlers.jpeg" :METHOD :POST)
