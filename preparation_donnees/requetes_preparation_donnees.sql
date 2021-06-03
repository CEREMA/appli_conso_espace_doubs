-- REQUETE PREPARATION DONNEES POUR APPLI SHINY CONSO D'ESPACE DDT 25

set search_path to a_detc_20ce0148_artificialisation_ddt25, public ;

-- table des EPCI

CREATE TABLE n_adm_exp_cog_epci_025_2020
AS
SELECT * 
FROM r_admin_express.n_adm_exp_cog_epci_000_2020
WHERE code_epci IN (SELECT DISTINCT code_epci FROM r_admin_express.n_adm_exp_cog_commune_000_2020 WHERE insee_dep = '25');

-- table des communes

CREATE TABLE n_adm_exp_cog_commune_025_2020
AS
SELECT geom, nom_com, insee_com, code_epci FROM r_admin_express.n_adm_exp_cog_commune_000_2020 WHERE insee_dep = '25' ;

create index n_adm_exp_cog_commune_025_2020_geom_gist on n_adm_exp_cog_commune_025_2020 using gist(geom) ;

-- table des PNR

drop table if exists pnr_025 cascade ;
create table pnr_025 as
select 'pnrph' as id, 'PNR Pays Horloger' as nom, geom from pnr_doubs_horloger_projet
union
select 'pnrhj' as id, 'PNR Haut Jura' as nom, geom from pnr_haut_jura ;

-- table des SCOT (importée depuis ideo BFC)

update scot_025_2020
set id_scot = 'scot'||id::varchar ;

-- ajout de différents codes dans table des communes

alter table n_adm_exp_cog_commune_025_2020
add column code_scot varchar (5) ;

alter table n_adm_exp_cog_commune_025_2020
add column code_pnr varchar (5) ;

update n_adm_exp_cog_commune_025_2020 as com
set code_scot = id_scot
from scot_025_2000 as scot
where st_intersects(st_PointOnSurface(com.geom), scot.geom) ;

update n_adm_exp_cog_commune_025_2020 as com
set code_pnr = pnr.id
from pnr_025 as pnr
where st_intersects(st_PointOnSurface(com.geom), pnr.geom) ;

alter table n_adm_exp_cog_commune_025_2020
add column zones varchar(30) ;

update n_adm_exp_cog_commune_025_2020
set zones = 'dept' || code_epci || code_scot || code_pnr ; -- marche pas car champs NA

-- table des zonages d'étude

create table zonages_025 as
select 'dept' as id_zone, 'departement' as type_zone, 'Département du Doubs' as nom_zone
union
select code_epci as id_zone, 'epci' as type_zone, nom_epci as nom_zone
from n_adm_exp_cog_epci_025_2020
union
select id_scot as id_zone, 'scot' as type_zone, nom_scot as nom_zone
from scot_025_2020
union
select id as id_zone, 'pnr' as type_zone, nom as nom_zone
from pnr_025 ;

-- table du bati

create index batiment_025_geom_gist on batiment_025 using gist(geom) ;

alter table batiment_025
add column insee_com varchar(5) ;

update batiment_025 as bat
set insee_com = com.insee_com
from n_adm_exp_cog_commune_025_2020 as com
where st_intersects(st_centroid(bat.geom), com.geom) ;

-- table d'indicateurs communaux

drop table if exists indicateurs_com_025 cascade ;

create table indicateurs_com_025 as

with surfaces as (
select code_insee::text as code_insee,
sum(case when datation = 1967 then surf_ha else 0 end) as senv68,
sum(case when datation = 1974 then surf_ha else 0 end) as senv75,
sum(case when datation = 1981 then surf_ha else 0 end) as senv82,
sum(case when datation = 1989 then surf_ha else 0 end) as senv90,
sum(case when datation = 1998 then surf_ha else 0 end) as senv99,
sum(case when datation = 2006 then surf_ha else 0 end) as senv07,
sum(case when datation = 2011 then surf_ha else 0 end) as senv12,
sum(case when datation = 2016 then surf_ha else 0 end) as senv17
from l_enveloppe_bati_025_historique
group by code_insee),

surfbati as (
select insee_com, round(sum(st_area(geom)))/10000 as sbati
from batiment_025
group by insee_com)

select com.insee_com, 
insee."P17_POP" as p17_pop,
insee."P12_POP" as p12_pop,
insee."P07_POP" as p07_pop,
insee."D99_POP" as d99_pop,
insee."D90_POP" as d90_pop,
insee."D82_POP" as d82_pop,
insee."D75_POP" as d75_pop,
insee."D68_POP" as d68_pop,
round(emploi."p17_emplt") as p17_emplt,
round(emploi."p12_emplt") as p12_emplt,
round(emploi."p07_emplt") as p07_emplt,
insee."P17_RSECOCC" as p17_rsec,
insee."P12_RSECOCC" as p12_rsec,
insee."P07_RSECOCC" as p07_rsec,
insee."D99_RSECOCC" as d99_rsec,
insee."D90_RSECOCC" as d90_rsec,
insee."D82_RSECOCC" as d82_rsec,
insee."D75_RSECOCC" as d75_rsec,
insee."D68_RSECOCC" as d68_rsec,
senv68, senv75, senv82, senv90, senv99, senv07, senv12, senv17,
sbati,
arthab0919, men12, men17, menhab1217,
st_area(envnd.geom)/10000 as senvnd
from n_adm_exp_cog_commune_025_2020 as com
left join insee_com_025_historique as insee on com.insee_com = insee."CODGEO"
left join insee_emploi_025_2017 as emploi on com.insee_com = emploi.codgeo
left join surfaces on com.insee_com = surfaces.code_insee
left join surfbati on com.insee_com = surfbati.insee_com
left join l_enveloppe_bati_025_2017 as envnd on com.insee_com = envnd.code_insee::text
left join obs_artif_conso_com_2009_2019 as artif on com.insee_com = artif.idcom ;


