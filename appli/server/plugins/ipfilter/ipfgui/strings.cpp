/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

char **CurST;

char* ST[][29] = {
	{
		"IP Filter settings",
		"Add rule...","Delete rule",
		"No chain",
		"Rules (order is important)","Default action, if no rule matched","Move up","Move down",
		"IP","Mask","Action","Name",
		"Filter rule","Rule name","IP","Netmask","Action","Apply to","Current chain","All chains","OK","Cancel",
		"Delete rule","Remove the following rule","from","All chains","Current chain","Cancel",
		"IP Filter"
	},
	{
		"Configuration du module de filtrage IP",
		"Ajouter...","Supprimer",
		"Pas de chaînes",
		"Règles (l'ordre est significatif)","Action par défaut","Monter","Descendre",
		"IP","Masque","Action","Nom",
		"Règle de filtrage","Nom de la règle","Adresse IP","Masque","Action","Appliquer à","Cette chaîne uniquement","Toutes les chaînes","OK","Annuler",
		"Supprimer la règle","Supprimer cette règle","de","Toutes les chaînes","La chaîne courante","Annuler",
		"Filter IP"
	}
	};
