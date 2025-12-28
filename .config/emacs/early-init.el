;; -*- lexical-binding: t; -*-

;; hide visual elements
(menu-bar-mode -1)
(when (functionp #'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (functionp #'tool-bar-mode)
  (tool-bar-mode -1))
