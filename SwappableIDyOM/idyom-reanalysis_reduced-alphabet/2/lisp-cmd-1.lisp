
                        (progn 
                        (defvar *idyom-root* "/Users/peter/idyom/")
                        (defvar *idyom-code-root*
                        (pathname "/Users/peter/quicklisp/local-projects/idyom/"))
                        (defvar *idyom-message-detail-level* 2)
                        (ql:quickload "idyom")
                        (clsql:connect '("/Users/peter/idyom/db/database.sqlite") :if-exists :old :database-type :sqlite3)
                        (eval (read-from-string "
                        (progn 
                        (idyom-db:delete-dataset 888888)
                        (idyom-db:delete-dataset 999999)
                        (idyom-db:import-data :txt \"/Users/peter/Dropbox/Academic/projects/entropy-vs-surprisal/output/stimuli/May-25/using-stim-corr-and-picking-best-sequences/idyom-reanalysis/2/new-training-data.txt\" \"Temporary Billboard training set\" 888888)
                        (idyom-db:import-data :txt \"/Users/peter/Dropbox/Academic/projects/entropy-vs-surprisal/output/stimuli/May-25/using-stim-corr-and-picking-best-sequences/idyom-reanalysis/2/new-test-data.txt\" \"Temporary Billboard test set\" 999999)
                        (utils:message \"Recomputing information-theoretic estimates...\")
                        (let ((viewpoints::*basic-types* '(:cpitch)))
                        (idyom:idyom 999999 '(cpitch) '(cpitch) :k 1
                        :models :both
                        :pretraining-ids '(888888)
                        :output-path \"/Users/peter/Dropbox/Academic/projects/entropy-vs-surprisal/output/stimuli/May-25/using-stim-corr-and-picking-best-sequences/idyom-reanalysis/2/recomputed-idyom-output\"
                        :use-resampling-set-cache? nil
                        :use-ltms-cache? nil)))"))
                        (sb-ext:quit))
