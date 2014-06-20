fun apply (ARG0, ARG1)  = 
    ((fn (ARG0, ARG1) =>
	 (SMLOG.mapS (List.map (fn ARGtc =>
				   ((fn _ =>
					((fn ARGres =>
					     ((fn _ =>
						  ARGres) (ARGtc SMLOG.Undo)
					      )
					     ) (((PRINTLmigt ((fn ARGt =>
								  (PRINTLint ARGt)
								  ), (fn ARGt =>
									 (PRINTLint ARGt)
									 )))
						     ARG0)
						, (PRINTLint ARG1)
						  ))
					) (ARGtc SMLOG.Redo)
				    )
				   ))
		     (RELapply (ARG0, ARG1) (fn _ =>
						()))
		     )
	 ) (((fn (Option.SOME ARG0) =>
		 ((EMBEDLmigt (((fn ARGt =>
				    (EMBEDLint ARGt)
				    ), (fn ARGt =>
					   (PROJLint ARGt)
					   )), ((fn ARGt =>
						    (EMBEDLint ARGt)
						    ), (fn ARGt =>
							   (PROJLint ARGt)
							   ))))
		      ARG0)
	       | Option.NONE =>
		 (Lmigt ((SMLOG.freshString ())
			 , (ref Option.NONE)
			   ))
		 ) ARG0)
	    , ((fn (Option.SOME ARG1) =>
		   (EMBEDLint ARG1)
		 | Option.NONE =>
		   (Lint ((SMLOG.freshString ())
			  , (ref Option.NONE)
			    ))
		   ) ARG1)
	      ))

