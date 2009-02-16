;;; style/mdwlist.el
;;; AUCTeX supoort for \usepackage{mdwlist}

(TeX-add-style-hook "mdwlist"
   (lambda ()
     
     (LaTeX-add-environments
      '("enumerate*" LaTeX-env-item)
      '("itemize*" LaTeX-env-item)
      '("description*" LaTeX-env-item)
      )

))
