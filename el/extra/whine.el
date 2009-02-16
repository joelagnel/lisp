;; Complaint generator for GNU Emacs

;; This file isn't part of much of anything.

;; This is distributed in the hope that it will be amusing,
;; but it has NO WARRANTY WHATEVER.  It probably has lots of bugs in it.
;; It may well do damage to all kinds of things, although I believe that it 
;; will not.  It will definitely will degrade system performance.  
;; The author takes no responsibility for anything.  

 
(defun abs (n)
   (if (< n 0) (- n) n))


(defvar unwhine-list nil)

(defun whine (key complaints)
  "Rebind KEY so that it utters one of the COMPLAINTS when
pressed.  unwhinify reverses the process on all keys."
  (let ((fn (global-key-binding key))
        whine-fn whine-list)
    (cond
     ((symbolp fn)
      (setq whine-list (intern (format "%s-whines"  fn)))
      (cond
       (t
        (not (fboundp whine-list))
        (setq whine-fn   (intern (format "%s-whiny"  fn)))
        (setq unwhine-list                ;unwhine-protect
              (cons (list 'global-set-key key (list 'quote fn))
                    unwhine-list))
        (set whine-list complaints)
        (eval
         (` (defun (, whine-fn) ()
              (interactive)
              (call-interactively  (quote (, fn)))
              (do-whine (, whine-list)))))))
      (global-set-key key whine-fn)
      ))))

(defun whine-say (s)
  (cond
   ((stringp s) s)
   ((symbolp s) (symbol-name s))
   ((consp s)
    (cond
     ((eq (car s) 'CAT)
      (mapconcat (function whine-say) (cdr s) " "))
     ((eq (car s) 'CATNS)
      (mapconcat (function whine-say) (cdr s) ""))
     ((eq (car s) 'lambda)
      (funcall s))
     (t
      (whine-say (random-element-of s)))))
   (t (prin1-to-string s))))
    

(defun do-whine (s)
  "Whines a string determined by S.
string --> whines S
list --> if the car is `CAT', it whines each other element of S and
               CATenates them;
         CATNS catenates without spaces
         else it picks an element and whines it
symbol --> whines its print representation
function --> funcalls it without arguments and whines the result."
  (message (whine-say s)))



(defun rand0 (n)
  (cond
   ((<= n 0) 0)
   (t (abs (% (random) n)))))

(defun  random-element-of (l) (nth (rand0 (length l)) l))

(defun whine-get-word ()
  (let (p)
    (save-excursion
      (backward-word 1)
      (setq p (point))
      (forward-word 1)
      (buffer-substring p (point)))))

(defun whinify ()
  "Make certain global-mode keys whine.  The command `unwhinify' makes 
them normal again."
  (interactive)
  (let* (
        (noun '("robot" "dweeb" "person" "child" "creep"
                "sleaze creature" "colorizer" "dentist"
                "snake" "communist" "capitalist" "fink"
                "panderer" "undergraduate" "dog" "thief" "Winkie"
                "millionaire" "porcupine" "nerd" "toad"
                "computer scientist" "mathematician" "logician"
                "waiter" "stenographer" "hacker" "MP" "Munchkin"
                "economist" "dog catcher" "hooker" "car dealer"
                "occultist" "Republican" "Democrat" "sheik"
                "architect" "masochist" "playboy" "devil worshipper"
                "owlbear" "pallbearer" "fungus" "superhero"
                "poisoner" "fungus" "congressman" "adulterer"
                "frog" "slug" "eel" "hyena" "pteranodon" "fish"
                "virus" "saint" "archangel" "astrophysicist"
                "biologist" "politician" "demagogue" "programmer"
                "disk jockey" "infant" "lawyer" "computer"
                 "limosine driver" "hamster" "rodent"))
        (adj   (`
                ("strange" "smelly" "Baltic" "masochistic"
                 "creepy" "illusory" "toxic" "idiotic"
                 "charismatic" "godly" "pneumatic" "well-painted"
                 "overweight" "underpaid" "overbearing" "underfed"
                 "robotic" "psychic" "deceptive" "lurking"
                 "radient" "turgid" "ironclad" "vicious"
                 "foolish" "nerdly" "isotonic" "bald"
                 "lurking" "iridescent" "polygonal" "freaky"
                 "drugged-out" "catatonic" "fainting" "typical"
                 "random" "horrible" "noxious" "slimy"
                 "puerile" "human" "toadlike" "wormlike"
                 "ghastly" "peurile" "goody-goody" "wombat-like"
                 "vapid" "stressed-out" "blessed" "saintly"
                 "heinous" "inedible" "droopy" "half-dead"
                 "blissed-out" "grossed-out" "knocked-out" "out-and-out"
                 (CATNS (, noun) "-like")
                 (CATNS (, noun) "-loving")
                 (CATNS (, noun) "-hating")
                 (CATNS (, noun) "-worshipping")
                 (CATNS (, noun) "-fearing")
                 (CATNS (, noun) "-hunting")
                 "political" "non-political" "communist"
                 "indecisive" "apathetic" "pathetic" "dyslexic"
                 "dreaming" "anorexic" "rich" "painted"
                 "orthodox" "lunatic" "fishlike" "inpure"
                 "virginal" "decrepit" "moronic" "despondent"
                 "hygenic" "unhygenic" "intolerable" "grease-coated"
                 "sluggish" "wimpy" "ponderous" "unlucky"
                 "brilliant" "unclothed" "heretical" "ironclad"
                 "undergraduate" "melting" "green" "nosy")))
        (whine-insults
         (list 'CATNS "You " adj " " adj " " noun "!"))
        (whine-pain
         (`
          (("Ouch!"
            "Don't do that!"
            "That hurts"
            "Get off my foot, you goon!"
            "Argh!"
            "Eek!"
            "Woe is me!"
            "Alas and alack!"
            "Have pity on me!"
            "Life sucks."
            "This jobs is *REALLY* *BORING*."
            "I knew there'd be days like this."
            "I hate being a computer!"
            (CAT ("I wish I were a"
                  "I should have been a"
                  "Why, oh why, didn't I become a"
                  "I wish I weren't a"
                  "Alas!  It is my ill fortune to be a"
                  "Have pity on me, for I am a"
                  )
                 (, adj)
                 (, noun))
            "Stop it!"
            "Oh! the pain, the pain!"
           (CAT
            ("Don't do that, you"
             "That hurt, you"
             "C'mon, you"
             "Stop acting like a"
             "Get off my foot, you"
             "You"
             "What a"
             "Don't make me call a"
             "Are you a human or a"
             (CAT
              "You"
              ("look" "act" "smell" "type" "program" "sound" "hesitate")
              "like a")
             (CAT
              "You must"
              ("love" "be" "know" "find" "worship" "have been")
              "a")
             "Don't press that key again, you")
            (, adj)
            (, adj)
            (, noun)
            )))))
        (insult-word
          (list 'CATNS
                (list
                 "Don't say `"
                 "Don't you know how to spell `"
                 "You don't mean `"
                 (list
                  'CAT
                  '("Only a")
                  adj
                  noun
                  '("would use a word like"
                    "would say")
                  " `"
                  )
                 "What do you mean by `"
                 "How can you say `")
                 (symbol-function 'whine-get-word)
                 "'"
                 )))
  (whine "" (append whine-pain
               '("Up, up, and away!"
                "This sure is an exciting line!"
                "Gee!  Let's do that again!"
                "Beginning of buffer"
                )))
  (whine "" (append whine-pain
               '("Don't make me go that way"
                "I don't want to go there!"
                "What an unpleasant line!"
                "Beginning of buffer"
                "End of buffer"
                "File not found"
                )))
  (whine "" (append whine-pain
                      '("Now what was that character again?"
                        "NOW look what you've done!"
                        "Buffer is read-only")))
  (whine "" '("Oh, no!  I've forgotten what I did!"))
  (whine "u" '("Oh, no!  I've forgotten what I did!"))
  (whine "" '("Please don't make me KILL again!"))
  (whine "" (append whine-pain
                      '("Forward ho!" "Oh, no!")))
  (whine "" (append whine-pain
                '("Please, Mr. Human Person, don't make me go that way!"
                "I am taken aback!"
                "Backwards ha!"
                "Catastrophic error -- world aborted."
                "Career aborted -- unable to restart."
                )))
  (whine "" (append whine-pain '("kill! Kill! KILL!")))
  (whine "b" (append whine-pain '("This buffer is horrible!"
                 "I don't want to be here!")))
  (whine "" '("You redrew the screen, you goon!"
                "No, you can't have a clear message line."
                "This is getting boring."
                "Give up and go play Rogue."
                "You just typed control-L to refresh the screen."
                ))

  (whine "%" '("Oh wow!  I couldn't have done it better myself!"))
  (whine "\C-i" '("Let's get the hell out of here!"))
  (whine "\e>" (append whine-pain '("It's off to the ends of the earth!")))
  (whine "\e<" (append whine-pain
                       '("It's off to the beginnings of the earth!")))
  (whine "$" insult-word)
  (whine " " insult-word)
  (whine "" whine-insults)
  (whine "b" whine-insults)
  (whine "f" whine-insults)
  (whine "" whine-insults)
  (whine "2" (append whine-pain '("That just cuts me up.")))
  (whine "x" '("You shouldn't have typed that command!"
                 "That's not a legal command."
                 ))
  (whine "\C-x\C-s" '("Filesystem not writable."
                      "File not saved."
                      "File system full."
                      "File system stupid."
                      "Good heavens.  You don't seriously intend me to write on *that*, do you!"
                      "File saved.  Hallelujah!"
                      "File accidentally deleted -- please retype."
                      ))
  (whine "" (append whine-pain '("If you touch that key again I shall explode!"
                "If you touch that key again I shall freak out!"
                "That's the space bar, not the return key!"
                "Press `c-c c-#' to abort."
                "I won't RETURN!"
                "You shouldn't have done that..."
                "Now look what you've done!")))
  (whine "" whine-pain)
  (whine "v" whine-pain)
  ))
                
(defun unwhinify ()
  "Hopefully, remove the whine-bindings on the whiny keys."
  (interactive)
  (mapcar (function eval) unwhine-list)
  (setq unwhine-list nil))
