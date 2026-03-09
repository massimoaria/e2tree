#' Credit Scoring Dataset
#'
#' A dataset containing socio-economic and banking information for 468 bank
#' clients, used to assess creditworthiness. All variables are categorical.
#'
#' @format A data frame with 468 rows and 12 columns:
#' \describe{
#'   \item{Type_of_client}{Credit evaluation outcome: \code{"Creditworthy"} or
#'     \code{"Non-Creditworthy"}.}
#'   \item{Client_Age}{Age class of the client (e.g., \code{"less than 23 years"},
#'     \code{"from 23 to 35 years"}, \code{"from 35 to 50 years"},
#'     \code{"over 50 years"}).}
#'   \item{Family_Situation}{Marital/family status of the client (e.g.,
#'     \code{"single"}, \code{"married"}, \code{"divorced"}).}
#'   \item{Account_Tenure}{Length of the client's relationship with the bank
#'     (e.g., \code{"1 year or less"}, \code{"from 2 to 5 years"},
#'     \code{"plus 12 years"}).}
#'   \item{Salary_Credited_to_Bank_Account}{Whether the client's salary is
#'     credited to the bank account (e.g., \code{"domicile salary"},
#'     \code{"no domicile salary"}).}
#'   \item{Ammount_of_Savings}{Client's level of savings (e.g.,
#'     \code{"no savings"}, \code{"less than 5 thousand"},
#'     \code{"from 5 to 30 thousand"}, \code{"more than 30 thousand"}).}
#'   \item{Customer_Occupation}{Employment category of the client (e.g.,
#'     \code{"employee"}, \code{"self-employed"}, \code{"retired"}).}
#'   \item{Average_Account_Balance}{Average balance held in the account (e.g.,
#'     \code{"from 2 to 5 thousand"}, \code{"more than 5 thousand"}).}
#'   \item{Average_Account_Turnover}{Average monthly turnover on the account
#'     (e.g., \code{"Less than 10 thousand"}, \code{"from 10 to 50 thousand"},
#'     \code{"more than 50 thousand"}).}
#'   \item{Credit_Card_Transaction_Count_Monthly}{Number of credit card
#'     transactions per month (e.g., \code{"less than 40"}, \code{"from 40 to 100"},
#'     \code{"more than 100"}).}
#'   \item{Authorized_Overdraft_Limit}{Whether the client has an authorized
#'     overdraft facility (\code{"Authorised"} or \code{"forbidden"}).}
#'   \item{Authorized_to_Issue_Bank_Checks}{Whether the client is authorized
#'     to issue bank checks (\code{"Authorised"} or \code{"forbidden"}).}
#' }
"credit"

