;;; init-docker.el --- Work with Docker and its tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Docker

;; I like to use Docker when I need to install various databases or other
;; services that only work on a particular operating system while keeping my operating system clean.
(use-package docker
  :mode "Dockerfile\\'"
  :delight "δ "
  :config
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))

(use-package dockerfile-mode :after docker)

(use-package docker-compose-mode :after docker)




(provide 'init-docker)
;;; init-docker.el ends here
