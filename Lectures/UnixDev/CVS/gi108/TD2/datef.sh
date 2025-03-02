#!/bin/bash
# la ligne ci-dessus indique que l'interpreteur de ce script est /bin/bash

# on reassigne les parametres positionnels ("positional parameters"),
# i.e. $1, $2, $3...normalement correspondant respectivement au
# premier, second, troisieme, ... parametres de la commande (ici
# datef.sh) lorsqu'elle est appelee, avec la commande "set"


set `date`

# on les stockes dans des variables au nom plus evocateur

JOUR_SEMAINE=$1
MOIS=$2
JOUR_MOIS=$3
HEURE=$4
ZONE=$5
ANNEE=$6

# on utilise la structure de controle "case" pour "traduire"

case $MOIS in

    Jan) MOIS="Janvier" ;;
    Feb) MOIS="Fevrier" ;;
    Mar) MOIS="Mars" ;;
    Apr) MOIS="Avril" ;;
    May) MOIS="Mai" ;;
    Jun) MOIS="Juin" ;;
    Jul) MOIS="Juillet" ;;
    Aug) MOIS="Aout" ;;
    Sep) MOIS="Septembre" ;;
    Oct) MOIS="Octobre" ;;
    Nov) MOIS="Novembre" ;;
    Dec) MOIS="Decembre" ;;

esac

case $JOUR_SEMAINE in

    Mon) JOUR_SEMAINE="Lundi" ;;
    Tue) JOUR_SEMAINE="Mardi" ;;
    Wen) JOUR_SEMAINE="Mercredi" ;;
    Thu) JOUR_SEMAINE="Jeudi" ;;
    Fri) JOUR_SEMAINE="Vendredi" ;;
    Sat) JOUR_SEMAINE="Samedi" ;;
    Sun) JOUR_SEMAINE="Dimanche" ;;

esac

ZONE="Europe Centrale"

# affichage en francais de la date

echo
echo "$JOUR_SEMAINE $JOUR_MOIS $MOIS $ANNEE $HEURE ($ZONE)"
echo

