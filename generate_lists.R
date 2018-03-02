# March 2, 2017

# Dane Van Domelen
# R file to create various lists for stocks package


# Load stocks package
library("stocks")



# Vanguard ETFs -----------------------------------------------------------

# 55 ETFs as of Oct. 18, 2017

# https://investor.vanguard.com/etf/list#/etf/asset-class/month-end-returns

# Bond ETFs
(vanguard_bond_etfs <-
   ticker_dates(tickers = c("EDV", "BIV", "VGIT", "BLV", "VGLT", "VMBS", "BSV",
                            "VTIP", "VGSH", "BND", "VCIT", "VCLT", "VCSH", 
                            "VTC", "VTEB"))[, 1: 2])

# Bond ETFs -> Treasury/Agency
(vanguard_treasury_etfs <-
    ticker_dates(tickers = c("EDV", "BIV", "VGIT", "BLV", "VGLT", "VMBS", "BSV",
                             "VGSH", "VTIP", "BND"))[, 1: 2])

# Bond ETFs -> Investment-grade
(vanguard_igrade_etfs <-
    ticker_dates(tickers = c("VCIT", "VCLT", "VCSH", "VTC"))[, 1: 2])

# Stocks ETFs
(vanguard_stock_etfs <-
    ticker_dates(tickers = c("VIG", "VUG", "VYM", "VV", "MGC", "MGK", "MGV",
                             "VOO", "VTI", "VTV", "VXF", "VO", "VOT", "VOE",
                             "VB", "VBK", "VBR"))[, 1: 2])

# Stock ETFs -> Large-cap
(vanguard_largecap_etfs <-
    ticker_dates(tickers = c("VIG", "VUG", "VYM", "VV", "MGC", "MGK", "MGV",
                             "VOO", "VTI", "VTV"))[, 1: 2])

# Stock ETFs -> Mid-cap
(vanguard_midcap_etfs <-
    ticker_dates(tickers = c("VXF", "VO", "VOT", "VOE"))[, 1: 2])

# Stock ETFs -> Small-cap
(vanguard_smallcap_etfs <-
    ticker_dates(tickers = c("VB", "VBK", "VBR"))[, 1: 2])

# International ETFs
(vanguard_international_etfs <-
    ticker_dates(tickers = c("VWOB", "VEU", "VSS", "VEA", "VWO", "VGK", "VPL",
                             "VNQI", "VIGI", "VYMI", "BNDX", "VXUS",
                             "VT"))[, 1: 2])

# Sector ETFs
(vanguard_sector_etfs <-
    ticker_dates(tickers = c("VCR", "VDC", "VDE", "VFH", "VHT", "VIS", "VGT",
                             "VAW", "VNQ", "VOX", "VPU"))[, 1: 2])

# All Vanguard ETFs
(vanguard_etfs <- rbind(vanguard_bond_etfs, vanguard_stock_etfs,
                        vanguard_international_etfs, vanguard_sector_etfs))



# Vanguard mutual funds ---------------------------------------------------

# Bond funds
(vanguard_bond_funds <-
   ticker_dates(tickers = c("VFIIX", "VIPSX", "VBILX", "VFITX", "VSIGX", 
                            "VBLTX", "VUSTX", "VLGSX", "VMBSX", "VBIRX",
                            "VSGBX", "VTAPX", "VFISX", "VSBSX", "VBTLX",
                            "VCORX", "VICSX", "VFICX", "VLTCX", "VWESX",
                            "VSCSX", "VFSTX", "VUBFX", "VWEHX", "VCAIX",
                            "VCITX", "VWAHX", "VWITX", "VMLTX", "VWLTX",
                            "VMATX", "VNJTX", "VNYTX", "VOHIX", "VPAIX",
                            "VWSTX", "VTEAX"))[, 1: 2])

# Bond funds -> Treasury/Agency
(vanguard_treasury_funds <-
    ticker_dates(tickers = c("VFIIX", "VIPSX", "VBILX", "VFITX", "VSIGX", 
                             "VBLTX", "VUSTX", "VLGSX", "VMBSX", "VBIRX",
                             "VSGBX", "VTAPX", "VFISX", "VSBSX", "VBTLX"))[, 1: 2])

# Bond funds -> Investment-grade
(vanguard_igrade_funds <-
    ticker_dates(tickers = c("VCORX", "VICSX", "VFICX", "VLTCX", "VWESX",
                             "VSCSX", "VFSTX", "VUBFX"))[, 1: 2])

# Bond funds -> Tax-exempt
(vanguard_taxexempt_bond_funds <-
    ticker_dates(tickers = c("VCAIX", "VCITX", "VWAHX", "VWITX", "VMLTX", 
                             "VWLTX", "VMATX", "VNJTX", "VNYTX", "VOHIX", 
                             "VPAIX", "VWSTX", "VTEAX"))[, 1: 2])

# Balanced funds
(vanguard_balanced_funds <-
    ticker_dates(tickers = c("VTXVX", "VTWNX", "VTTVX", "VTHRX", "VTTHX", 
                             "VFORX", "VTIVX", "VFIFX", "VFFVX", "VTTSX", 
                             "VLXVX", "VTINX", "VSCGX", "VASGX", "VASIX", 
                             "VSMGX", "VBIAX", "VCVSX", "VGSTX", "VTMFX", 
                             "VWINX", "VWELX", "VPGDX", "VGWIX", "VGWLX"))[, 1: 2])

# Balanced funds -> Target-date
(vanguard_targetdate_funds <-
    ticker_dates(tickers = c("VTENX", "VTXVX", "VTWNX", "VTTVX", "VTHRX",
                             "VTTHX", "VFORX", "VTIVX", "VFIFX", "VFFVX",
                             "VTTSX", "VLXVX", "VTINX"))[, 1: 2])

# Balanced funds -> Target-risk
(vanguard_targetrisk_funds <-
    ticker_dates(tickers = c("VSCGX", "VASGX", "VASIX", "VSMGX"))[, 1: 2])

# Balanced funds -> Traditional
(vanguard_traditional_funds <-
    ticker_dates(tickers = c("VBIAX", "VCVSX", "VGSTX", "VTMFX", "VWINX", 
                             "VWELX"))[, 1: 2])

# Stock funds
(vanguard_stock_funds <-
    ticker_dates(tickers = c("VFIAX", "VDEQX", "VDADX", "VDIGX", "VEIPX",
                             "VFTSX", "VQNPX", "VIGAX", "VHDYX", "VLCAX",
                             "VMRGX", "VPMAX", "VPCCX", "VTCLX", "VTSAX",
                             "VWUSX", "VUVLX", "VVIAX", "VWNDX", "VWNFX",
                             "VHCAX", "VCVLX", "VEXAX", "VMGRX", "VMGMX",
                             "VIMAX", "VMVAX", "VASVX", "VSEQX", "VEXPX",
                             "VEVFX", "VSGAX", "VSMAX", "VSIAX", "VSTCX",
                             "VTMSX", "VMVFX"))[, 1: 2])

# Stock funds -> Large-cap
(vanguard_largecap_funds <-
    ticker_dates(tickers = c("VFIAX", "VDEQX", "VDADX", "VDIGX", "VEIPX",
                             "VFTSX", "VQNPX", "VIGAX", "VHDYX", "VLCAX",
                             "VMRGX", "VPMAX", "VPCCX", "VTCLX", "VTSAX",
                             "VWUSX", "VUVLX", "VVIAX", "VWNDX", "VWNFX"))[, 1: 2])

# Stock funds -> Mid-cap
(vanguard_midcap_funds <-
    ticker_dates(tickers = c("VHCAX", "VCVLX", "VEXAX", "VMGRX", "VMGMX",
                             "VIMAX", "VMVAX", "VASVX", "VSEQX"))[, 1: 2])

# Stock funds -> Small-cap
(vanguard_smallcap_funds <-
    ticker_dates(tickers = c("VEXPX", "VEVFX", "VSGAX", "VSMAX", "VSIAX", 
                             "VSTCX", "VTMSX"))[, 1: 2])

# Vanguard international funds
(vanguard_international_funds <-
    ticker_dates(tickers = c("VTMGX", "VEMBX", "VGAVX", "VMMSX", "VEMAX", 
                             "VEUSX", "VFWAX", "VFSVX", "VGRLX", "VIAAX", 
                             "VINEX", "VWIGX", "VIHAX", "VTRIX", "VPADX", 
                             "VTABX", "VTIAX", "VHGEX", "VTWSX"))[, 1: 2])

# Vanguard sector funds
(vanguard_sector_funds <-
    ticker_dates(tickers = c("VGENX", "VGHCX", "VGPMX", "VGSLX"))[, 1: 2])

# All Vanguard mutual funds
(vanguard_funds <-
    rbind(vanguard_bond_funds, vanguard_balanced_funds, vanguard_stock_funds,
          vanguard_international_funds, vanguard_sector_funds))



# High yield ETFs ---------------------------------------------------------

# 47 ETFs as of Oct. 18, 2017

# From: http://etfdb.com/etfdb-category/high-yield-bonds/#etfs&sort_name=assets_under_management&sort_order=desc&page=2

(highyield_etfs <-
  ticker_dates(tickers = c("ANGL", "BKLN", "BLHY", "BSJI", "BSJJ",
                           "BSJK", "BSJL", "BSJM", "BSJN", "BSJO", 
                           "BSJP", "CJNK", "EMHY", "FALN", "FTSL", 
                           "GHYG", "HHYX", "HYDB", "HYDW", "HYEM", 
                           "HYG", "HYGH", "HYHG", "HYIH", "HYLB", 
                           "HYLD", "HYLS", "HYLV", "HYND", "HYS", 
                           "HYUP", "HYXE", "HYXU", "HYZD", "IHY", 
                           "JNK", "JPHY", "PGHY", "PHB", "SFHY", 
                           "SHYG", "SHYL", "SJNK", "SNLN", "SOVB",
                           "SRLN", "USHY", "WFHY", "WYDE", "YLD"))[, 1: 2])



# Sector SPDR ETFs  -----------------------------------------------------

# 10 ETFs as of March 2, 2018

# From: http://www.sectorspdr.com/sectorspdr/sectors
(sector_spdr_etfs <-
   ticker_dates(tickers = c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB",
                            "XLRE", "XLK", "XLU"))[, 1: 2])



# 100 largest market cap ETFs ---------------------------------------------

# From: http://etfdb.com/compare/market-cap/
setwd("C:/Users/Dane/Google Drive/github/stocks")
(largest_etfs <-
    ticker_dates(tickers = read.csv("largest100.csv", header = F, 
                                    stringsAsFactors = FALSE)[, 1])[, 1: 2])


# Save datasets -----------------------------------------------------------

setwd("C:/Users/Dane/Google Drive/github/stocks/data")
save(vanguard_bond_etfs, file = "vanguard_bond_etfs.rda")
save(vanguard_treasury_etfs, file = "vanguard_treasury_etfs.rda")
save(vanguard_igrade_etfs, file = "vanguard_igrade_etfs.rda")
save(vanguard_stock_etfs, file = "vanguard_stock_etfs.rda")
save(vanguard_largecap_etfs, file = "vanguard_largecap_etfs.rda")
save(vanguard_midcap_etfs, file = "vanguard_midcap_etfs.rda")
save(vanguard_smallcap_etfs, file = "vanguard_smallcap_etfs.rda")
save(vanguard_international_etfs, file = "vanguard_international_etfs.rda")
save(vanguard_sector_etfs, file = "vanguard_sector_etfs.rda")
save(vanguard_etfs, file = "vanguard_etfs.rda")

save(vanguard_bond_funds, file = "vanguard_bond_funds.rda")
save(vanguard_treasury_funds, file = "vanguard_treasury_funds.rda")
save(vanguard_igrade_funds, file = "vanguard_igrade_funds.rda")
save(vanguard_taxexempt_bond_funds, file = "vanguard_taxexempt_bond_funds.rda")
save(vanguard_balanced_funds, file = "vanguard_balanced_funds.rda")
save(vanguard_targetdate_funds, file = "vanguard_targetdate_funds.rda")
save(vanguard_targetrisk_funds, file = "vanguard_targetrisk_funds.rda")
save(vanguard_traditional_funds, file = "vanguard_traditional_funds.rda")
save(vanguard_stock_funds, file = "vanguard_stock_funds.rda")
save(vanguard_largecap_funds, file = "vanguard_largecap_funds.rda")
save(vanguard_midcap_funds, file = "vanguard_midcap_funds.rda")
save(vanguard_smallcap_funds, file = "vanguard_smallcap_funds.rda")
save(vanguard_international_funds, file = "vanguard_international_funds.rda")
save(vanguard_sector_funds, file = "vanguard_sector_funds.rda")

save(vanguard_funds, file = "vanguard_funds.rda")

save(highyield_etfs, file = "highyield_etfs.rda")

save(sector_spdr_etfs, file = "sector_spdr_etfs.rda")

save(largest_etfs, file = "largest_etfs.rda")
