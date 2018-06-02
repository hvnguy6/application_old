
source("/home/ubuntu/app_v1/fun_optionPricer.R")
        
FoldLists1 = c("AAPL","AMD","ATVI","BABA","BA", "BAC", "COST", "CRM", "FB", "GS", "INTC", "JD", "JPM", "KODK",  "MSFT", "MU", "NFLX", "NVDA", "NKE", "PYPL", "SPY",  "SPLK", "TSLA", "UVXY", "XLF", "XLK", "XLP", "WB", "WMT")



for (i in 1:length(FoldLists1)) {
        optionPricer(FoldLists1[i], RF=.01068, PriceRange = .25)
}