#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Experiment differences between the three courses
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+
#+http://www.dwoll.de/rexrepos/posts/anovaMixed.html
#+https://stats.stackexchange.com/questions/58745/using-lmer-for-repeated-measures-linear-mixed-effect-model
#+
#+So, the idea is to take the average difference for each person (from his 100 'repeated measurement"  i.e., simulations)
#+and to to put all these differences together in one data frame with an inficator from which subject does the measruement come from
#+#so the Subject would be the IV. Another option might to do an MANOVA where the DV is a composite variable comprised of
#+#1. the (average)difference between the true and re-estimated theta; the number 