object mrna{
	import rosalind.CodonTable
	
	val data = "MMSYQNIEVRHSVPCYSHTVAVKVCNQTMTLHSLMPRSVPDAIRQCNCAKDREAQIDYRIVFYFLYLSQCSNIDTKWLHTEPVPDVTLWSEVHNPGNMEDLCDNHGCPPQQVNGALVTAQCTYLSTQGNFVWSTQAGDFIAHQWPAVEWRMGTYITSVFTDFYCWVNTISYGWKLGFHINLHCALAQVAEKPIKCLCFWMCIHPIRDVSALQGDFHAVGNYQHFPATAMQPWKVTCTILWGKYDECDGVDTMAQRKECHYASYVCMRHPANPVIMFIAPVHSVTYRVNKYRYVCYGFMAIKVKIMDWDPQVDPEDWWPAVTCGEEKYIQKHSFLSHYKWRMEQCVWNNEQYVNDNNDYWEEFDQREHTMWCGFRQRWHISVEYEYIMPTTKKVCFRCLEIHVQATPWNPFILIEAATIWGRHYFNQNQFNKIFNKESIHPCEVNWCSYIKHYVIMHHQHKRAWVNWHARLTNYWRDNSDMSYQMPPVDRGHCKCCDCKYEFDAGFERNLNLDFEDYFVGPLYTDGHEANKYQTMQQEMYCGGFRKDGQWAYRDAHCCYLENECRRIRYNGKFDIIRASTCSPFFTGGSHACVYEWCVAGWIMYHAAFDEQNTMYQAALMPAYRRTRLAIVLQNPHFKDCPHIWRTGPLGHKNMMAPEFCQLSRSELYRFAVMVSMFCTMNICHDPSHKTVNKVMHYFSIFFGSQMEEEAAACLISAVPKNWWPKHFVFALLNGLIHAPMPCFSQQICHYCEGDLWYIHHERDVQWTPKWVTWIYCIMDGCSYYTKLPIDANKYRTLLVECKPFRLEMSYTEWGPDPYWKTDNENEMVKDYETNICHNMTCHAITNHAHTFREGFFNHFIITMCAFDCYMVGWEMVQCLHEAAMIDQNMFGHCHGMFTCMNCKVIPEPRICHFNGCDGCLAKGMNCAIFWFVMMFGTMVYIDPVLGQCISRFDFPCGGMWSQSHWNHCSEPNNTRAHRQWEQMRAEPEWHYAAQPEINKK"
                                                  //> data  : String = MMSYQNIEVRHSVPCYSHTVAVKVCNQTMTLHSLMPRSVPDAIRQCNCAKDREAQIDY
                                                  //| RIVFYFLYLSQCSNIDTKWLHTEPVPDVTLWSEVHNPGNMEDLCDNHGCPPQQVNGALVTAQCTYLSTQGNFVWS
                                                  //| TQAGDFIAHQWPAVEWRMGTYITSVFTDFYCWVNTISYGWKLGFHINLHCALAQVAEKPIKCLCFWMCIHPIRDV
                                                  //| SALQGDFHAVGNYQHFPATAMQPWKVTCTILWGKYDECDGVDTMAQRKECHYASYVCMRHPANPVIMFIAPVHSV
                                                  //| TYRVNKYRYVCYGFMAIKVKIMDWDPQVDPEDWWPAVTCGEEKYIQKHSFLSHYKWRMEQCVWNNEQYVNDNNDY
                                                  //| WEEFDQREHTMWCGFRQRWHISVEYEYIMPTTKKVCFRCLEIHVQATPWNPFILIEAATIWGRHYFNQNQFNKIF
                                                  //| NKESIHPCEVNWCSYIKHYVIMHHQHKRAWVNWHARLTNYWRDNSDMSYQMPPVDRGHCKCCDCKYEFDAGFERN
                                                  //| LNLDFEDYFVGPLYTDGHEANKYQTMQQEMYCGGFRKDGQWAYRDAHCCYLENECRRIRYNGKFDIIRASTCSPF
                                                  //| FTGGSHACVYEWCVAGWIMYHAAFDEQNTMYQAALMPAYRRTRLAIVLQNPHFKDCPHIWRTGPLGHKNMMAPEF
                                                  //| CQLSRSELYRFAVMVSMFCTMNICHDPSHKTVNKVMHYFSIFFGSQMEEEAAACLISAVPKNWWPKHFVFALLNG
                                                  //| LIHAPMPCFSQQICHYCEGDLWYIHHERDVQWTPKWVTWIYCIMDGCSYYTKLPIDANKYRTLLVECKPFRLEMS
                                                  //| YTEWGPDPYWKTDNENEMVKDYETNICHNMTCHAITNHAHTFREGFFNHFIITMCAFDCYMVGWEMVQCLHEAAM
                                                  //| IDQNMFGHCHGMFTCMNCKVIPE
                                                  //| Output exceeds cutoff limit.
	
	def codonVariations(n:Long, data:String, codonTable: Map[Char, Int]) = {
		def itdata(data:List[Char], acc:Long):Long = data match {
			case Nil => acc
			case x::xs => itdata(xs, (acc * codonTable(x)) % n)
		}
		itdata((data+CodonTable.stopChar).toList, 1)
	}                                         //> codonVariations: (n: Long, data: String, codonTable: Map[Char,Int])Long
	
	val n = 1000000l                          //> n  : Long = 1000000
	codonVariations(n, data, CodonTable.aminoAcidsCounts)
                                                  //> res0: Long = 131904
}