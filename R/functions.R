#' @title demi_perimetre
#' @description Calcule le demi périmètre d'un triangle
#' @param a Longueur d'un côté du triangle
#' @param b Longueur d'un côté du triangle
#' @param c Longueur d'un côté du triangle
#' @return Le demi périmètre du triangle
#' @noRd
#'
demi_perimetre <- function(a, b, c) {
  (a + b + c) / 2
}

#' @title heron
#' @description Calcule l'aire d'un triangle à partir de ses côtés
#' @param a Longueur d'un côté du triangle
#' @param b Longueur d'un côté du triangle
#' @param c Longueur d'un côté du triangle
#' @return L'aire du triangle
#' @examples
#' heron(3, 4, 5)
#' heron(6, 8, 10)
#'
heron <- function(a, b, c) {
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(c)) {
    stop("Tous les arguments doivent être des nombres.")
  }
  if (a <= 0 || b <= 0 || c <= 0) {
    stop("Aucune des valeurs ne peut être négative ou nulle.")
  }
  p <- demi_perimetre(a, b, c)
  if (p <= 0) {
    stop("Le demi périmètre doit être strictement positif.")
  }
  sqrt(p * (p - a) * (p - b) * (p - c))
}


##### TEST #####

# Test de la fonction demi_perimetre


# Test 1 :
a <- 2
b <- 3
c <- 4
resultat_perimetre <- demi_perimetre(a, b, c)
if (resultat_perimetre == (a + b + c) / 2) {
  print("Test 1 : succès")
} else {
  print("Test 1 : échec")
}

# Test 1 :
a <- 4
b <- 5
c <- 6
resultat_perimetre <- demi_perimetre(a, b, c)
if (resultat_perimetre == (a + b + c) / 2) {
  print("Test 2 : succès")
} else {
  print("Test 2 : échec")
}


# Test de la fonction heron

# Test 1 :
a <- 3
b <- 4
c <- 5
resultat_attendu <- 6
resultat_calcule <- heron(a, b, c)
if (resultat_attendu == resultat_calcule) {
  print("Le résultat attendu correspond au résultat calculé")
} else {
  print("Le résultat attendu ne correspond pas au résultat calculé")
}

# Test 2 :


a <- 10
b <- 9
c <- 7
resultat_attendu <- 30.59412
resultat_calcule <- round(heron(a, b, c),5)
if (resultat_attendu == resultat_calcule) {
  print("Le résultat attendu correspond au résultat calculé")
} else {
  print("Le résultat attendu ne correspond pas au résultat calculé")
}


# Test 3 : Vérifier le retour d'une erreur pour une valeur négative
if (inherits(try(heron(a, b, val_neg)), "try-error")) {
  print("Test 3 : succès")
} else {
  print("Test 3 : échec")
}

# Test 4 : Vérifier le retour d'une erreur pour une valeur non numérique
if (inherits(try(heron(a, b, val_non_num)), "try-error")) {
  print("Test 4 : succès")
} else {
  print("Test 4 : échec")
}




