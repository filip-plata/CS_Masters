all: total_score.txt

results.txt: result01.txt result02.txt result03.txt result04.txt result05.txt result06.txt result07.txt result08.txt result09.txt result10.txt
	@rm -f results.txt
	@head -n1 result01.txt >> results.txt
	@head -n1 result02.txt >> results.txt
	@head -n1 result03.txt >> results.txt
	@head -n1 result04.txt >> results.txt
	@head -n1 result05.txt >> results.txt
	@head -n1 result06.txt >> results.txt
	@head -n1 result07.txt >> results.txt
	@head -n1 result08.txt >> results.txt
	@head -n1 result09.txt >> results.txt
	@head -n1 result10.txt >> results.txt 
	
total_score.txt: results.txt
	@awk -f cumulate.awk results.txt > total_score.txt

result%.txt: test%.lagda.md
	@./score.sh $*

submission.clean.lagda.md: submission.lagda.md 
	@# clean up markdown source obtained from nextjournal or binder
	@./sanitise.sh submission.lagda.md > submission.clean.lagda.md

test%.lagda.md: submission.clean.lagda.md
	@./extract.sh $* submission.clean.lagda.md > $@

clean:
	rm -f result*.txt
	rm -fr submission.clean.lagda.md
	rm -f test*.lagda.md
	rm -f *.agdai