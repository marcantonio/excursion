;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 directory-files
 ((("/excursion:electron:~/excursion") '("." ".." ".git" ".gitignore" "Cargo.lock" "Cargo.toml" "elisp" "rustfmt.toml" "scripts" "src" "target"))
  (("/excursion:electron:~/excursion" t) '("." ".." "/home/mas/excursion/.git" "/home/mas/excursion/.gitignore" "/home/mas/excursion/Cargo.lock" "/home/mas/excursion/Cargo.toml" "/home/mas/excursion/elisp" "/home/mas/excursion/rustfmt.toml" "/home/mas/excursion/scripts" "/home/mas/excursion/src" "/home/mas/excursion/target"))
  (("/excursion:electron:~/excursion" nil "^.git") '(".git" ".gitignore"))
  (("/excursion:electron:~/excursion" nil nil t) '("." ".." "rustfmt.toml" "Cargo.toml" "elisp" "target" ".gitignore" ".git" "src" "Cargo.lock" "scripts"))
  (("/excursion:electron:~/excursion" nil nil nil 3) '("." ".." ".git"))
  (("/excursion:electron:~/excursion" t "argo\." t 1) '("/home/mas/excursion/Cargo.toml"))))

(provide 'directory-files-test)
