:- encoding('UTF-8').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(iso_3166_2).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Generated ISO 3166-2 subdivision facts extracted from the Debian iso-codes machine-readable JSON snapshot.',
		remarks is [
			'Source URL' - 'https://salsa.debian.org/iso-codes-team/iso-codes/-/raw/main/data/iso_3166-2.json',
			'Generated entries' - '5046'
		]
	]).

	:- public(subdivision/4).
	:- mode(subdivision(?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(subdivision/4, [
		comment is 'Generated ISO 3166-2 subdivision fact table.',
		argnames is ['Code', 'CountryAlpha2', 'Name', 'Category']
	]).

	subdivision('ad-02', 'ad', 'Canillo', 'Parish').
	subdivision('ad-03', 'ad', 'Encamp', 'Parish').
	subdivision('ad-04', 'ad', 'La Massana', 'Parish').
	subdivision('ad-05', 'ad', 'Ordino', 'Parish').
	subdivision('ad-06', 'ad', 'Sant Julià de Lòria', 'Parish').
	subdivision('ad-07', 'ad', 'Andorra la Vella', 'Parish').
	subdivision('ad-08', 'ad', 'Escaldes-Engordany', 'Parish').
	subdivision('ae-aj', 'ae', '‘Ajmān', 'Emirate').
	subdivision('ae-az', 'ae', 'Abū Z̧aby', 'Emirate').
	subdivision('ae-du', 'ae', 'Dubayy', 'Emirate').
	subdivision('ae-fu', 'ae', 'Al Fujayrah', 'Emirate').
	subdivision('ae-rk', 'ae', 'Ra’s al Khaymah', 'Emirate').
	subdivision('ae-sh', 'ae', 'Ash Shāriqah', 'Emirate').
	subdivision('ae-uq', 'ae', 'Umm al Qaywayn', 'Emirate').
	subdivision('af-bal', 'af', 'Balkh', 'Province').
	subdivision('af-bam', 'af', 'Bāmyān', 'Province').
	subdivision('af-bdg', 'af', 'Bādghīs', 'Province').
	subdivision('af-bds', 'af', 'Badakhshān', 'Province').
	subdivision('af-bgl', 'af', 'Baghlān', 'Province').
	subdivision('af-day', 'af', 'Dāykundī', 'Province').
	subdivision('af-fra', 'af', 'Farāh', 'Province').
	subdivision('af-fyb', 'af', 'Fāryāb', 'Province').
	subdivision('af-gha', 'af', 'Ghaznī', 'Province').
	subdivision('af-gho', 'af', 'Ghōr', 'Province').
	subdivision('af-hel', 'af', 'Helmand', 'Province').
	subdivision('af-her', 'af', 'Herāt', 'Province').
	subdivision('af-jow', 'af', 'Jowzjān', 'Province').
	subdivision('af-kab', 'af', 'Kābul', 'Province').
	subdivision('af-kan', 'af', 'Kandahār', 'Province').
	subdivision('af-kap', 'af', 'Kāpīsā', 'Province').
	subdivision('af-kdz', 'af', 'Kunduz', 'Province').
	subdivision('af-kho', 'af', 'Khōst', 'Province').
	subdivision('af-knr', 'af', 'Kunaṟ', 'Province').
	subdivision('af-lag', 'af', 'Laghmān', 'Province').
	subdivision('af-log', 'af', 'Lōgar', 'Province').
	subdivision('af-nan', 'af', 'Nangarhār', 'Province').
	subdivision('af-nim', 'af', 'Nīmrōz', 'Province').
	subdivision('af-nur', 'af', 'Nūristān', 'Province').
	subdivision('af-pan', 'af', 'Panjshayr', 'Province').
	subdivision('af-par', 'af', 'Parwān', 'Province').
	subdivision('af-pia', 'af', 'Paktiyā', 'Province').
	subdivision('af-pka', 'af', 'Paktīkā', 'Province').
	subdivision('af-sam', 'af', 'Samangān', 'Province').
	subdivision('af-sar', 'af', 'Sar-e Pul', 'Province').
	subdivision('af-tak', 'af', 'Takhār', 'Province').
	subdivision('af-uru', 'af', 'Uruzgān', 'Province').
	subdivision('af-war', 'af', 'Wardak', 'Province').
	subdivision('af-zab', 'af', 'Zābul', 'Province').
	subdivision('ag-03', 'ag', 'Saint George', 'Parish').
	subdivision('ag-04', 'ag', 'Saint John', 'Parish').
	subdivision('ag-05', 'ag', 'Saint Mary', 'Parish').
	subdivision('ag-06', 'ag', 'Saint Paul', 'Parish').
	subdivision('ag-07', 'ag', 'Saint Peter', 'Parish').
	subdivision('ag-08', 'ag', 'Saint Philip', 'Parish').
	subdivision('ag-10', 'ag', 'Barbuda', 'Dependency').
	subdivision('ag-11', 'ag', 'Redonda', 'Dependency').
	subdivision('al-01', 'al', 'Berat', 'County').
	subdivision('al-02', 'al', 'Durrës', 'County').
	subdivision('al-03', 'al', 'Elbasan', 'County').
	subdivision('al-04', 'al', 'Fier', 'County').
	subdivision('al-05', 'al', 'Gjirokastër', 'County').
	subdivision('al-06', 'al', 'Korçë', 'County').
	subdivision('al-07', 'al', 'Kukës', 'County').
	subdivision('al-08', 'al', 'Lezhë', 'County').
	subdivision('al-09', 'al', 'Dibër', 'County').
	subdivision('al-10', 'al', 'Shkodër', 'County').
	subdivision('al-11', 'al', 'Tiranë', 'County').
	subdivision('al-12', 'al', 'Vlorë', 'County').
	subdivision('am-ag', 'am', 'Aragac̣otn', 'Region').
	subdivision('am-ar', 'am', 'Ararat', 'Region').
	subdivision('am-av', 'am', 'Armavir', 'Region').
	subdivision('am-er', 'am', 'Erevan', 'City').
	subdivision('am-gr', 'am', 'Geġark''unik''', 'Region').
	subdivision('am-kt', 'am', 'Kotayk''', 'Region').
	subdivision('am-lo', 'am', 'Loṙi', 'Region').
	subdivision('am-sh', 'am', 'Širak', 'Region').
	subdivision('am-su', 'am', 'Syunik''', 'Region').
	subdivision('am-tv', 'am', 'Tavuš', 'Region').
	subdivision('am-vd', 'am', 'Vayoć Jor', 'Region').
	subdivision('ao-bgo', 'ao', 'Bengo', 'Province').
	subdivision('ao-bgu', 'ao', 'Benguela', 'Province').
	subdivision('ao-bie', 'ao', 'Bié', 'Province').
	subdivision('ao-cab', 'ao', 'Cabinda', 'Province').
	subdivision('ao-ccu', 'ao', 'Cuando Cubango', 'Province').
	subdivision('ao-cnn', 'ao', 'Cunene', 'Province').
	subdivision('ao-cno', 'ao', 'Cuanza-Norte', 'Province').
	subdivision('ao-cus', 'ao', 'Cuanza-Sul', 'Province').
	subdivision('ao-hua', 'ao', 'Huambo', 'Province').
	subdivision('ao-hui', 'ao', 'Huíla', 'Province').
	subdivision('ao-lno', 'ao', 'Lunda-Norte', 'Province').
	subdivision('ao-lsu', 'ao', 'Lunda-Sul', 'Province').
	subdivision('ao-lua', 'ao', 'Luanda', 'Province').
	subdivision('ao-mal', 'ao', 'Malange', 'Province').
	subdivision('ao-mox', 'ao', 'Moxico', 'Province').
	subdivision('ao-nam', 'ao', 'Namibe', 'Province').
	subdivision('ao-uig', 'ao', 'Uíge', 'Province').
	subdivision('ao-zai', 'ao', 'Zaire', 'Province').
	subdivision('ar-a', 'ar', 'Salta', 'Province').
	subdivision('ar-b', 'ar', 'Buenos Aires', 'Province').
	subdivision('ar-c', 'ar', 'Ciudad Autónoma de Buenos Aires', 'City').
	subdivision('ar-d', 'ar', 'San Luis', 'Province').
	subdivision('ar-e', 'ar', 'Entre Ríos', 'Province').
	subdivision('ar-f', 'ar', 'La Rioja', 'Province').
	subdivision('ar-g', 'ar', 'Santiago del Estero', 'Province').
	subdivision('ar-h', 'ar', 'Chaco', 'Province').
	subdivision('ar-j', 'ar', 'San Juan', 'Province').
	subdivision('ar-k', 'ar', 'Catamarca', 'Province').
	subdivision('ar-l', 'ar', 'La Pampa', 'Province').
	subdivision('ar-m', 'ar', 'Mendoza', 'Province').
	subdivision('ar-n', 'ar', 'Misiones', 'Province').
	subdivision('ar-p', 'ar', 'Formosa', 'Province').
	subdivision('ar-q', 'ar', 'Neuquén', 'Province').
	subdivision('ar-r', 'ar', 'Río Negro', 'Province').
	subdivision('ar-s', 'ar', 'Santa Fe', 'Province').
	subdivision('ar-t', 'ar', 'Tucumán', 'Province').
	subdivision('ar-u', 'ar', 'Chubut', 'Province').
	subdivision('ar-v', 'ar', 'Tierra del Fuego', 'Province').
	subdivision('ar-w', 'ar', 'Corrientes', 'Province').
	subdivision('ar-x', 'ar', 'Córdoba', 'Province').
	subdivision('ar-y', 'ar', 'Jujuy', 'Province').
	subdivision('ar-z', 'ar', 'Santa Cruz', 'Province').
	subdivision('at-1', 'at', 'Burgenland', 'State').
	subdivision('at-2', 'at', 'Kärnten', 'State').
	subdivision('at-3', 'at', 'Niederösterreich', 'State').
	subdivision('at-4', 'at', 'Oberösterreich', 'State').
	subdivision('at-5', 'at', 'Salzburg', 'State').
	subdivision('at-6', 'at', 'Steiermark', 'State').
	subdivision('at-7', 'at', 'Tirol', 'State').
	subdivision('at-8', 'at', 'Vorarlberg', 'State').
	subdivision('at-9', 'at', 'Wien', 'State').
	subdivision('au-act', 'au', 'Australian Capital Territory', 'Territory').
	subdivision('au-nsw', 'au', 'New South Wales', 'State').
	subdivision('au-nt', 'au', 'Northern Territory', 'Territory').
	subdivision('au-qld', 'au', 'Queensland', 'State').
	subdivision('au-sa', 'au', 'South Australia', 'State').
	subdivision('au-tas', 'au', 'Tasmania', 'State').
	subdivision('au-vic', 'au', 'Victoria', 'State').
	subdivision('au-wa', 'au', 'Western Australia', 'State').
	subdivision('az-abs', 'az', 'Abşeron', 'Rayon').
	subdivision('az-aga', 'az', 'Ağstafa', 'Rayon').
	subdivision('az-agc', 'az', 'Ağcabədi', 'Rayon').
	subdivision('az-agm', 'az', 'Ağdam', 'Rayon').
	subdivision('az-ags', 'az', 'Ağdaş', 'Rayon').
	subdivision('az-agu', 'az', 'Ağsu', 'Rayon').
	subdivision('az-ast', 'az', 'Astara', 'Rayon').
	subdivision('az-ba', 'az', 'Bakı', 'Municipality').
	subdivision('az-bab', 'az', 'Babək', 'Rayon').
	subdivision('az-bal', 'az', 'Balakən', 'Rayon').
	subdivision('az-bar', 'az', 'Bərdə', 'Rayon').
	subdivision('az-bey', 'az', 'Beyləqan', 'Rayon').
	subdivision('az-bil', 'az', 'Biləsuvar', 'Rayon').
	subdivision('az-cab', 'az', 'Cəbrayıl', 'Rayon').
	subdivision('az-cal', 'az', 'Cəlilabad', 'Rayon').
	subdivision('az-cul', 'az', 'Culfa', 'Rayon').
	subdivision('az-das', 'az', 'Daşkəsən', 'Rayon').
	subdivision('az-fuz', 'az', 'Füzuli', 'Rayon').
	subdivision('az-ga', 'az', 'Gəncə', 'Municipality').
	subdivision('az-gad', 'az', 'Gədəbəy', 'Rayon').
	subdivision('az-gor', 'az', 'Goranboy', 'Rayon').
	subdivision('az-goy', 'az', 'Göyçay', 'Rayon').
	subdivision('az-gyg', 'az', 'Göygöl', 'Rayon').
	subdivision('az-hac', 'az', 'Hacıqabul', 'Rayon').
	subdivision('az-imi', 'az', 'İmişli', 'Rayon').
	subdivision('az-ism', 'az', 'İsmayıllı', 'Rayon').
	subdivision('az-kal', 'az', 'Kəlbəcər', 'Rayon').
	subdivision('az-kan', 'az', 'Kǝngǝrli', 'Rayon').
	subdivision('az-kur', 'az', 'Kürdəmir', 'Rayon').
	subdivision('az-la', 'az', 'Lənkəran', 'Municipality').
	subdivision('az-lac', 'az', 'Laçın', 'Rayon').
	subdivision('az-lan', 'az', 'Lənkəran', 'Rayon').
	subdivision('az-ler', 'az', 'Lerik', 'Rayon').
	subdivision('az-mas', 'az', 'Masallı', 'Rayon').
	subdivision('az-mi', 'az', 'Mingəçevir', 'Municipality').
	subdivision('az-na', 'az', 'Naftalan', 'Municipality').
	subdivision('az-nef', 'az', 'Neftçala', 'Rayon').
	subdivision('az-nv', 'az', 'Naxçıvan', 'Municipality').
	subdivision('az-nx', 'az', 'Naxçıvan', 'Autonomous republic').
	subdivision('az-ogu', 'az', 'Oğuz', 'Rayon').
	subdivision('az-ord', 'az', 'Ordubad', 'Rayon').
	subdivision('az-qab', 'az', 'Qəbələ', 'Rayon').
	subdivision('az-qax', 'az', 'Qax', 'Rayon').
	subdivision('az-qaz', 'az', 'Qazax', 'Rayon').
	subdivision('az-qba', 'az', 'Quba', 'Rayon').
	subdivision('az-qbi', 'az', 'Qubadlı', 'Rayon').
	subdivision('az-qob', 'az', 'Qobustan', 'Rayon').
	subdivision('az-qus', 'az', 'Qusar', 'Rayon').
	subdivision('az-sa', 'az', 'Şəki', 'Municipality').
	subdivision('az-sab', 'az', 'Sabirabad', 'Rayon').
	subdivision('az-sad', 'az', 'Sədərək', 'Rayon').
	subdivision('az-sah', 'az', 'Şahbuz', 'Rayon').
	subdivision('az-sak', 'az', 'Şəki', 'Rayon').
	subdivision('az-sal', 'az', 'Salyan', 'Rayon').
	subdivision('az-sar', 'az', 'Şərur', 'Rayon').
	subdivision('az-sat', 'az', 'Saatlı', 'Rayon').
	subdivision('az-sbn', 'az', 'Şabran', 'Rayon').
	subdivision('az-siy', 'az', 'Siyəzən', 'Rayon').
	subdivision('az-skr', 'az', 'Şəmkir', 'Rayon').
	subdivision('az-sm', 'az', 'Sumqayıt', 'Municipality').
	subdivision('az-smi', 'az', 'Şamaxı', 'Rayon').
	subdivision('az-smx', 'az', 'Samux', 'Rayon').
	subdivision('az-sr', 'az', 'Şirvan', 'Municipality').
	subdivision('az-sus', 'az', 'Şuşa', 'Rayon').
	subdivision('az-tar', 'az', 'Tərtər', 'Rayon').
	subdivision('az-tov', 'az', 'Tovuz', 'Rayon').
	subdivision('az-uca', 'az', 'Ucar', 'Rayon').
	subdivision('az-xa', 'az', 'Xankəndi', 'Municipality').
	subdivision('az-xac', 'az', 'Xaçmaz', 'Rayon').
	subdivision('az-xci', 'az', 'Xocalı', 'Rayon').
	subdivision('az-xiz', 'az', 'Xızı', 'Rayon').
	subdivision('az-xvd', 'az', 'Xocavənd', 'Rayon').
	subdivision('az-yar', 'az', 'Yardımlı', 'Rayon').
	subdivision('az-ye', 'az', 'Yevlax', 'Municipality').
	subdivision('az-yev', 'az', 'Yevlax', 'Rayon').
	subdivision('az-zan', 'az', 'Zəngilan', 'Rayon').
	subdivision('az-zaq', 'az', 'Zaqatala', 'Rayon').
	subdivision('az-zar', 'az', 'Zərdab', 'Rayon').
	subdivision('ba-bih', 'ba', 'Federacija Bosne i Hercegovine', 'Entity').
	subdivision('ba-brc', 'ba', 'Brčko distrikt', 'District with special status').
	subdivision('ba-srp', 'ba', 'Republika Srpska', 'Entity').
	subdivision('bb-01', 'bb', 'Christ Church', 'Parish').
	subdivision('bb-02', 'bb', 'Saint Andrew', 'Parish').
	subdivision('bb-03', 'bb', 'Saint George', 'Parish').
	subdivision('bb-04', 'bb', 'Saint James', 'Parish').
	subdivision('bb-05', 'bb', 'Saint John', 'Parish').
	subdivision('bb-06', 'bb', 'Saint Joseph', 'Parish').
	subdivision('bb-07', 'bb', 'Saint Lucy', 'Parish').
	subdivision('bb-08', 'bb', 'Saint Michael', 'Parish').
	subdivision('bb-09', 'bb', 'Saint Peter', 'Parish').
	subdivision('bb-10', 'bb', 'Saint Philip', 'Parish').
	subdivision('bb-11', 'bb', 'Saint Thomas', 'Parish').
	subdivision('bd-01', 'bd', 'Bandarban', 'District').
	subdivision('bd-02', 'bd', 'Barguna', 'District').
	subdivision('bd-03', 'bd', 'Bogura', 'District').
	subdivision('bd-04', 'bd', 'Brahmanbaria', 'District').
	subdivision('bd-05', 'bd', 'Bagerhat', 'District').
	subdivision('bd-06', 'bd', 'Barishal', 'District').
	subdivision('bd-07', 'bd', 'Bhola', 'District').
	subdivision('bd-08', 'bd', 'Cumilla', 'District').
	subdivision('bd-09', 'bd', 'Chandpur', 'District').
	subdivision('bd-10', 'bd', 'Chattogram', 'District').
	subdivision('bd-11', 'bd', 'Cox''s Bazar', 'District').
	subdivision('bd-12', 'bd', 'Chuadanga', 'District').
	subdivision('bd-13', 'bd', 'Dhaka', 'District').
	subdivision('bd-14', 'bd', 'Dinajpur', 'District').
	subdivision('bd-15', 'bd', 'Faridpur', 'District').
	subdivision('bd-16', 'bd', 'Feni', 'District').
	subdivision('bd-17', 'bd', 'Gopalganj', 'District').
	subdivision('bd-18', 'bd', 'Gazipur', 'District').
	subdivision('bd-19', 'bd', 'Gaibandha', 'District').
	subdivision('bd-20', 'bd', 'Habiganj', 'District').
	subdivision('bd-21', 'bd', 'Jamalpur', 'District').
	subdivision('bd-22', 'bd', 'Jashore', 'District').
	subdivision('bd-23', 'bd', 'Jhenaidah', 'District').
	subdivision('bd-24', 'bd', 'Joypurhat', 'District').
	subdivision('bd-25', 'bd', 'Jhalakathi', 'District').
	subdivision('bd-26', 'bd', 'Kishoreganj', 'District').
	subdivision('bd-27', 'bd', 'Khulna', 'District').
	subdivision('bd-28', 'bd', 'Kurigram', 'District').
	subdivision('bd-29', 'bd', 'Khagrachhari', 'District').
	subdivision('bd-30', 'bd', 'Kushtia', 'District').
	subdivision('bd-31', 'bd', 'Lakshmipur', 'District').
	subdivision('bd-32', 'bd', 'Lalmonirhat', 'District').
	subdivision('bd-33', 'bd', 'Manikganj', 'District').
	subdivision('bd-34', 'bd', 'Mymensingh', 'District').
	subdivision('bd-35', 'bd', 'Munshiganj', 'District').
	subdivision('bd-36', 'bd', 'Madaripur', 'District').
	subdivision('bd-37', 'bd', 'Magura', 'District').
	subdivision('bd-38', 'bd', 'Moulvibazar', 'District').
	subdivision('bd-39', 'bd', 'Meherpur', 'District').
	subdivision('bd-40', 'bd', 'Narayanganj', 'District').
	subdivision('bd-41', 'bd', 'Netrakona', 'District').
	subdivision('bd-42', 'bd', 'Narsingdi', 'District').
	subdivision('bd-43', 'bd', 'Narail', 'District').
	subdivision('bd-44', 'bd', 'Natore', 'District').
	subdivision('bd-45', 'bd', 'Chapai Nawabganj', 'District').
	subdivision('bd-46', 'bd', 'Nilphamari', 'District').
	subdivision('bd-47', 'bd', 'Noakhali', 'District').
	subdivision('bd-48', 'bd', 'Naogaon', 'District').
	subdivision('bd-49', 'bd', 'Pabna', 'District').
	subdivision('bd-50', 'bd', 'Pirojpur', 'District').
	subdivision('bd-51', 'bd', 'Patuakhali', 'District').
	subdivision('bd-52', 'bd', 'Panchagarh', 'District').
	subdivision('bd-53', 'bd', 'Rajbari', 'District').
	subdivision('bd-54', 'bd', 'Rajshahi', 'District').
	subdivision('bd-55', 'bd', 'Rangpur', 'District').
	subdivision('bd-56', 'bd', 'Rangamati', 'District').
	subdivision('bd-57', 'bd', 'Sherpur', 'District').
	subdivision('bd-58', 'bd', 'Satkhira', 'District').
	subdivision('bd-59', 'bd', 'Sirajganj', 'District').
	subdivision('bd-60', 'bd', 'Sylhet', 'District').
	subdivision('bd-61', 'bd', 'Sunamganj', 'District').
	subdivision('bd-62', 'bd', 'Shariatpur', 'District').
	subdivision('bd-63', 'bd', 'Tangail', 'District').
	subdivision('bd-64', 'bd', 'Thakurgaon', 'District').
	subdivision('bd-a', 'bd', 'Barishal', 'Division').
	subdivision('bd-b', 'bd', 'Chattogram', 'Division').
	subdivision('bd-c', 'bd', 'Dhaka', 'Division').
	subdivision('bd-d', 'bd', 'Khulna', 'Division').
	subdivision('bd-e', 'bd', 'Rajshahi', 'Division').
	subdivision('bd-f', 'bd', 'Rangpur', 'Division').
	subdivision('bd-g', 'bd', 'Sylhet', 'Division').
	subdivision('bd-h', 'bd', 'Mymensingh', 'Division').
	subdivision('be-bru', 'be', 'Bruxelles-Capitale, Région de', 'Region').
	subdivision('be-van', 'be', 'Antwerpen', 'Province').
	subdivision('be-vbr', 'be', 'Vlaams-Brabant', 'Province').
	subdivision('be-vlg', 'be', 'Vlaams Gewest', 'Region').
	subdivision('be-vli', 'be', 'Limburg', 'Province').
	subdivision('be-vov', 'be', 'Oost-Vlaanderen', 'Province').
	subdivision('be-vwv', 'be', 'West-Vlaanderen', 'Province').
	subdivision('be-wal', 'be', 'wallonne, Région', 'Region').
	subdivision('be-wbr', 'be', 'Brabant wallon', 'Province').
	subdivision('be-wht', 'be', 'Hainaut', 'Province').
	subdivision('be-wlg', 'be', 'Liège', 'Province').
	subdivision('be-wlx', 'be', 'Luxembourg', 'Province').
	subdivision('be-wna', 'be', 'Namur', 'Province').
	subdivision('bf-01', 'bf', 'Boucle du Mouhoun', 'Region').
	subdivision('bf-02', 'bf', 'Cascades', 'Region').
	subdivision('bf-03', 'bf', 'Centre', 'Region').
	subdivision('bf-04', 'bf', 'Centre-Est', 'Region').
	subdivision('bf-05', 'bf', 'Centre-Nord', 'Region').
	subdivision('bf-06', 'bf', 'Centre-Ouest', 'Region').
	subdivision('bf-07', 'bf', 'Centre-Sud', 'Region').
	subdivision('bf-08', 'bf', 'Est', 'Region').
	subdivision('bf-09', 'bf', 'Hauts-Bassins', 'Region').
	subdivision('bf-10', 'bf', 'Nord', 'Region').
	subdivision('bf-11', 'bf', 'Plateau-Central', 'Region').
	subdivision('bf-12', 'bf', 'Sahel', 'Region').
	subdivision('bf-13', 'bf', 'Sud-Ouest', 'Region').
	subdivision('bf-bal', 'bf', 'Balé', 'Province').
	subdivision('bf-bam', 'bf', 'Bam', 'Province').
	subdivision('bf-ban', 'bf', 'Banwa', 'Province').
	subdivision('bf-baz', 'bf', 'Bazèga', 'Province').
	subdivision('bf-bgr', 'bf', 'Bougouriba', 'Province').
	subdivision('bf-blg', 'bf', 'Boulgou', 'Province').
	subdivision('bf-blk', 'bf', 'Boulkiemdé', 'Province').
	subdivision('bf-com', 'bf', 'Comoé', 'Province').
	subdivision('bf-gan', 'bf', 'Ganzourgou', 'Province').
	subdivision('bf-gna', 'bf', 'Gnagna', 'Province').
	subdivision('bf-gou', 'bf', 'Gourma', 'Province').
	subdivision('bf-hou', 'bf', 'Houet', 'Province').
	subdivision('bf-iob', 'bf', 'Ioba', 'Province').
	subdivision('bf-kad', 'bf', 'Kadiogo', 'Province').
	subdivision('bf-ken', 'bf', 'Kénédougou', 'Province').
	subdivision('bf-kmd', 'bf', 'Komondjari', 'Province').
	subdivision('bf-kmp', 'bf', 'Kompienga', 'Province').
	subdivision('bf-kop', 'bf', 'Koulpélogo', 'Province').
	subdivision('bf-kos', 'bf', 'Kossi', 'Province').
	subdivision('bf-kot', 'bf', 'Kouritenga', 'Province').
	subdivision('bf-kow', 'bf', 'Kourwéogo', 'Province').
	subdivision('bf-ler', 'bf', 'Léraba', 'Province').
	subdivision('bf-lor', 'bf', 'Loroum', 'Province').
	subdivision('bf-mou', 'bf', 'Mouhoun', 'Province').
	subdivision('bf-nam', 'bf', 'Namentenga', 'Province').
	subdivision('bf-nao', 'bf', 'Nahouri', 'Province').
	subdivision('bf-nay', 'bf', 'Nayala', 'Province').
	subdivision('bf-nou', 'bf', 'Noumbiel', 'Province').
	subdivision('bf-oub', 'bf', 'Oubritenga', 'Province').
	subdivision('bf-oud', 'bf', 'Oudalan', 'Province').
	subdivision('bf-pas', 'bf', 'Passoré', 'Province').
	subdivision('bf-pon', 'bf', 'Poni', 'Province').
	subdivision('bf-sen', 'bf', 'Séno', 'Province').
	subdivision('bf-sis', 'bf', 'Sissili', 'Province').
	subdivision('bf-smt', 'bf', 'Sanmatenga', 'Province').
	subdivision('bf-sng', 'bf', 'Sanguié', 'Province').
	subdivision('bf-som', 'bf', 'Soum', 'Province').
	subdivision('bf-sor', 'bf', 'Sourou', 'Province').
	subdivision('bf-tap', 'bf', 'Tapoa', 'Province').
	subdivision('bf-tui', 'bf', 'Tuy', 'Province').
	subdivision('bf-yag', 'bf', 'Yagha', 'Province').
	subdivision('bf-yat', 'bf', 'Yatenga', 'Province').
	subdivision('bf-zir', 'bf', 'Ziro', 'Province').
	subdivision('bf-zon', 'bf', 'Zondoma', 'Province').
	subdivision('bf-zou', 'bf', 'Zoundwéogo', 'Province').
	subdivision('bg-01', 'bg', 'Blagoevgrad', 'District').
	subdivision('bg-02', 'bg', 'Burgas', 'District').
	subdivision('bg-03', 'bg', 'Varna', 'District').
	subdivision('bg-04', 'bg', 'Veliko Tarnovo', 'District').
	subdivision('bg-05', 'bg', 'Vidin', 'District').
	subdivision('bg-06', 'bg', 'Vratsa', 'District').
	subdivision('bg-07', 'bg', 'Gabrovo', 'District').
	subdivision('bg-08', 'bg', 'Dobrich', 'District').
	subdivision('bg-09', 'bg', 'Kardzhali', 'District').
	subdivision('bg-10', 'bg', 'Kyustendil', 'District').
	subdivision('bg-11', 'bg', 'Lovech', 'District').
	subdivision('bg-12', 'bg', 'Montana', 'District').
	subdivision('bg-13', 'bg', 'Pazardzhik', 'District').
	subdivision('bg-14', 'bg', 'Pernik', 'District').
	subdivision('bg-15', 'bg', 'Pleven', 'District').
	subdivision('bg-16', 'bg', 'Plovdiv', 'District').
	subdivision('bg-17', 'bg', 'Razgrad', 'District').
	subdivision('bg-18', 'bg', 'Ruse', 'District').
	subdivision('bg-19', 'bg', 'Silistra', 'District').
	subdivision('bg-20', 'bg', 'Sliven', 'District').
	subdivision('bg-21', 'bg', 'Smolyan', 'District').
	subdivision('bg-22', 'bg', 'Sofia (stolitsa)', 'District').
	subdivision('bg-23', 'bg', 'Sofia', 'District').
	subdivision('bg-24', 'bg', 'Stara Zagora', 'District').
	subdivision('bg-25', 'bg', 'Targovishte', 'District').
	subdivision('bg-26', 'bg', 'Haskovo', 'District').
	subdivision('bg-27', 'bg', 'Shumen', 'District').
	subdivision('bg-28', 'bg', 'Yambol', 'District').
	subdivision('bh-13', 'bh', 'Al ‘Āşimah', 'Governorate').
	subdivision('bh-14', 'bh', 'Al Janūbīyah', 'Governorate').
	subdivision('bh-15', 'bh', 'Al Muḩarraq', 'Governorate').
	subdivision('bh-17', 'bh', 'Ash Shamālīyah', 'Governorate').
	subdivision('bi-bb', 'bi', 'Bubanza', 'Province').
	subdivision('bi-bl', 'bi', 'Bujumbura Rural', 'Province').
	subdivision('bi-bm', 'bi', 'Bujumbura Mairie', 'Province').
	subdivision('bi-br', 'bi', 'Bururi', 'Province').
	subdivision('bi-ca', 'bi', 'Cankuzo', 'Province').
	subdivision('bi-ci', 'bi', 'Cibitoke', 'Province').
	subdivision('bi-gi', 'bi', 'Gitega', 'Province').
	subdivision('bi-ki', 'bi', 'Kirundo', 'Province').
	subdivision('bi-kr', 'bi', 'Karuzi', 'Province').
	subdivision('bi-ky', 'bi', 'Kayanza', 'Province').
	subdivision('bi-ma', 'bi', 'Makamba', 'Province').
	subdivision('bi-mu', 'bi', 'Muramvya', 'Province').
	subdivision('bi-mw', 'bi', 'Mwaro', 'Province').
	subdivision('bi-my', 'bi', 'Muyinga', 'Province').
	subdivision('bi-ng', 'bi', 'Ngozi', 'Province').
	subdivision('bi-rm', 'bi', 'Rumonge', 'Province').
	subdivision('bi-rt', 'bi', 'Rutana', 'Province').
	subdivision('bi-ry', 'bi', 'Ruyigi', 'Province').
	subdivision('bj-ak', 'bj', 'Atacora', 'Department').
	subdivision('bj-al', 'bj', 'Alibori', 'Department').
	subdivision('bj-aq', 'bj', 'Atlantique', 'Department').
	subdivision('bj-bo', 'bj', 'Borgou', 'Department').
	subdivision('bj-co', 'bj', 'Collines', 'Department').
	subdivision('bj-do', 'bj', 'Donga', 'Department').
	subdivision('bj-ko', 'bj', 'Couffo', 'Department').
	subdivision('bj-li', 'bj', 'Littoral', 'Department').
	subdivision('bj-mo', 'bj', 'Mono', 'Department').
	subdivision('bj-ou', 'bj', 'Ouémé', 'Department').
	subdivision('bj-pl', 'bj', 'Plateau', 'Department').
	subdivision('bj-zo', 'bj', 'Zou', 'Department').
	subdivision('bn-be', 'bn', 'Belait', 'District').
	subdivision('bn-bm', 'bn', 'Brunei-Muara', 'District').
	subdivision('bn-te', 'bn', 'Temburong', 'District').
	subdivision('bn-tu', 'bn', 'Tutong', 'District').
	subdivision('bo-b', 'bo', 'El Beni', 'Department').
	subdivision('bo-c', 'bo', 'Cochabamba', 'Department').
	subdivision('bo-h', 'bo', 'Chuquisaca', 'Department').
	subdivision('bo-l', 'bo', 'La Paz', 'Department').
	subdivision('bo-n', 'bo', 'Pando', 'Department').
	subdivision('bo-o', 'bo', 'Oruro', 'Department').
	subdivision('bo-p', 'bo', 'Potosí', 'Department').
	subdivision('bo-s', 'bo', 'Santa Cruz', 'Department').
	subdivision('bo-t', 'bo', 'Tarija', 'Department').
	subdivision('bq-bo', 'bq', 'Bonaire', 'Special municipality').
	subdivision('bq-sa', 'bq', 'Saba', 'Special municipality').
	subdivision('bq-se', 'bq', 'Sint Eustatius', 'Special municipality').
	subdivision('br-ac', 'br', 'Acre', 'State').
	subdivision('br-al', 'br', 'Alagoas', 'State').
	subdivision('br-am', 'br', 'Amazonas', 'State').
	subdivision('br-ap', 'br', 'Amapá', 'State').
	subdivision('br-ba', 'br', 'Bahia', 'State').
	subdivision('br-ce', 'br', 'Ceará', 'State').
	subdivision('br-df', 'br', 'Distrito Federal', 'Federal district').
	subdivision('br-es', 'br', 'Espírito Santo', 'State').
	subdivision('br-go', 'br', 'Goiás', 'State').
	subdivision('br-ma', 'br', 'Maranhão', 'State').
	subdivision('br-mg', 'br', 'Minas Gerais', 'State').
	subdivision('br-ms', 'br', 'Mato Grosso do Sul', 'State').
	subdivision('br-mt', 'br', 'Mato Grosso', 'State').
	subdivision('br-pa', 'br', 'Pará', 'State').
	subdivision('br-pb', 'br', 'Paraíba', 'State').
	subdivision('br-pe', 'br', 'Pernambuco', 'State').
	subdivision('br-pi', 'br', 'Piauí', 'State').
	subdivision('br-pr', 'br', 'Paraná', 'State').
	subdivision('br-rj', 'br', 'Rio de Janeiro', 'State').
	subdivision('br-rn', 'br', 'Rio Grande do Norte', 'State').
	subdivision('br-ro', 'br', 'Rondônia', 'State').
	subdivision('br-rr', 'br', 'Roraima', 'State').
	subdivision('br-rs', 'br', 'Rio Grande do Sul', 'State').
	subdivision('br-sc', 'br', 'Santa Catarina', 'State').
	subdivision('br-se', 'br', 'Sergipe', 'State').
	subdivision('br-sp', 'br', 'São Paulo', 'State').
	subdivision('br-to', 'br', 'Tocantins', 'State').
	subdivision('bs-ak', 'bs', 'Acklins', 'District').
	subdivision('bs-bi', 'bs', 'Bimini', 'District').
	subdivision('bs-bp', 'bs', 'Black Point', 'District').
	subdivision('bs-by', 'bs', 'Berry Islands', 'District').
	subdivision('bs-ce', 'bs', 'Central Eleuthera', 'District').
	subdivision('bs-ci', 'bs', 'Cat Island', 'District').
	subdivision('bs-ck', 'bs', 'Crooked Island and Long Cay', 'District').
	subdivision('bs-co', 'bs', 'Central Abaco', 'District').
	subdivision('bs-cs', 'bs', 'Central Andros', 'District').
	subdivision('bs-eg', 'bs', 'East Grand Bahama', 'District').
	subdivision('bs-ex', 'bs', 'Exuma', 'District').
	subdivision('bs-fp', 'bs', 'City of Freeport', 'District').
	subdivision('bs-gc', 'bs', 'Grand Cay', 'District').
	subdivision('bs-hi', 'bs', 'Harbour Island', 'District').
	subdivision('bs-ht', 'bs', 'Hope Town', 'District').
	subdivision('bs-in', 'bs', 'Inagua', 'District').
	subdivision('bs-li', 'bs', 'Long Island', 'District').
	subdivision('bs-mc', 'bs', 'Mangrove Cay', 'District').
	subdivision('bs-mg', 'bs', 'Mayaguana', 'District').
	subdivision('bs-mi', 'bs', 'Moore''s Island', 'District').
	subdivision('bs-ne', 'bs', 'North Eleuthera', 'District').
	subdivision('bs-no', 'bs', 'North Abaco', 'District').
	subdivision('bs-np', 'bs', 'New Providence', 'Island').
	subdivision('bs-ns', 'bs', 'North Andros', 'District').
	subdivision('bs-rc', 'bs', 'Rum Cay', 'District').
	subdivision('bs-ri', 'bs', 'Ragged Island', 'District').
	subdivision('bs-sa', 'bs', 'South Andros', 'District').
	subdivision('bs-se', 'bs', 'South Eleuthera', 'District').
	subdivision('bs-so', 'bs', 'South Abaco', 'District').
	subdivision('bs-ss', 'bs', 'San Salvador', 'District').
	subdivision('bs-sw', 'bs', 'Spanish Wells', 'District').
	subdivision('bs-wg', 'bs', 'West Grand Bahama', 'District').
	subdivision('bt-11', 'bt', 'Paro', 'District').
	subdivision('bt-12', 'bt', 'Chhukha', 'District').
	subdivision('bt-13', 'bt', 'Haa', 'District').
	subdivision('bt-14', 'bt', 'Samtse', 'District').
	subdivision('bt-15', 'bt', 'Thimphu', 'District').
	subdivision('bt-21', 'bt', 'Tsirang', 'District').
	subdivision('bt-22', 'bt', 'Dagana', 'District').
	subdivision('bt-23', 'bt', 'Punakha', 'District').
	subdivision('bt-24', 'bt', 'Wangdue Phodrang', 'District').
	subdivision('bt-31', 'bt', 'Sarpang', 'District').
	subdivision('bt-32', 'bt', 'Trongsa', 'District').
	subdivision('bt-33', 'bt', 'Bumthang', 'District').
	subdivision('bt-34', 'bt', 'Zhemgang', 'District').
	subdivision('bt-41', 'bt', 'Trashigang', 'District').
	subdivision('bt-42', 'bt', 'Monggar', 'District').
	subdivision('bt-43', 'bt', 'Pema Gatshel', 'District').
	subdivision('bt-44', 'bt', 'Lhuentse', 'District').
	subdivision('bt-45', 'bt', 'Samdrup Jongkhar', 'District').
	subdivision('bt-ga', 'bt', 'Gasa', 'District').
	subdivision('bt-ty', 'bt', 'Trashi Yangtse', 'District').
	subdivision('bw-ce', 'bw', 'Central', 'District').
	subdivision('bw-ch', 'bw', 'Chobe', 'District').
	subdivision('bw-fr', 'bw', 'Francistown', 'City').
	subdivision('bw-ga', 'bw', 'Gaborone', 'City').
	subdivision('bw-gh', 'bw', 'Ghanzi', 'District').
	subdivision('bw-jw', 'bw', 'Jwaneng', 'Town').
	subdivision('bw-kg', 'bw', 'Kgalagadi', 'District').
	subdivision('bw-kl', 'bw', 'Kgatleng', 'District').
	subdivision('bw-kw', 'bw', 'Kweneng', 'District').
	subdivision('bw-lo', 'bw', 'Lobatse', 'Town').
	subdivision('bw-ne', 'bw', 'North East', 'District').
	subdivision('bw-nw', 'bw', 'North West', 'District').
	subdivision('bw-se', 'bw', 'South East', 'District').
	subdivision('bw-so', 'bw', 'Southern', 'District').
	subdivision('bw-sp', 'bw', 'Selibe Phikwe', 'Town').
	subdivision('bw-st', 'bw', 'Sowa Town', 'Town').
	subdivision('by-br', 'by', 'Bresckaja voblasć', 'Oblast').
	subdivision('by-hm', 'by', 'Horad Minsk', 'City').
	subdivision('by-ho', 'by', 'Homieĺskaja voblasć', 'Oblast').
	subdivision('by-hr', 'by', 'Hrodzienskaja voblasć', 'Oblast').
	subdivision('by-ma', 'by', 'Mahilioŭskaja voblasć', 'Oblast').
	subdivision('by-mi', 'by', 'Minskaja voblasć', 'Oblast').
	subdivision('by-vi', 'by', 'Viciebskaja voblasć', 'Oblast').
	subdivision('bz-bz', 'bz', 'Belize', 'District').
	subdivision('bz-cy', 'bz', 'Cayo', 'District').
	subdivision('bz-czl', 'bz', 'Corozal', 'District').
	subdivision('bz-ow', 'bz', 'Orange Walk', 'District').
	subdivision('bz-sc', 'bz', 'Stann Creek', 'District').
	subdivision('bz-tol', 'bz', 'Toledo', 'District').
	subdivision('ca-ab', 'ca', 'Alberta', 'Province').
	subdivision('ca-bc', 'ca', 'British Columbia', 'Province').
	subdivision('ca-mb', 'ca', 'Manitoba', 'Province').
	subdivision('ca-nb', 'ca', 'New Brunswick', 'Province').
	subdivision('ca-nl', 'ca', 'Newfoundland and Labrador', 'Province').
	subdivision('ca-ns', 'ca', 'Nova Scotia', 'Province').
	subdivision('ca-nt', 'ca', 'Northwest Territories', 'Territory').
	subdivision('ca-nu', 'ca', 'Nunavut', 'Territory').
	subdivision('ca-on', 'ca', 'Ontario', 'Province').
	subdivision('ca-pe', 'ca', 'Prince Edward Island', 'Province').
	subdivision('ca-qc', 'ca', 'Quebec', 'Province').
	subdivision('ca-sk', 'ca', 'Saskatchewan', 'Province').
	subdivision('ca-yt', 'ca', 'Yukon', 'Territory').
	subdivision('cd-bc', 'cd', 'Kongo Central', 'Province').
	subdivision('cd-bu', 'cd', 'Bas-Uélé', 'Province').
	subdivision('cd-eq', 'cd', 'Équateur', 'Province').
	subdivision('cd-hk', 'cd', 'Haut-Katanga', 'Province').
	subdivision('cd-hl', 'cd', 'Haut-Lomami', 'Province').
	subdivision('cd-hu', 'cd', 'Haut-Uélé', 'Province').
	subdivision('cd-it', 'cd', 'Ituri', 'Province').
	subdivision('cd-kc', 'cd', 'Kasaï Central', 'Province').
	subdivision('cd-ke', 'cd', 'Kasaï Oriental', 'Province').
	subdivision('cd-kg', 'cd', 'Kwango', 'Province').
	subdivision('cd-kl', 'cd', 'Kwilu', 'Province').
	subdivision('cd-kn', 'cd', 'Kinshasa', 'City').
	subdivision('cd-ks', 'cd', 'Kasaï', 'Province').
	subdivision('cd-lo', 'cd', 'Lomami', 'Province').
	subdivision('cd-lu', 'cd', 'Lualaba', 'Province').
	subdivision('cd-ma', 'cd', 'Maniema', 'Province').
	subdivision('cd-mn', 'cd', 'Mai-Ndombe', 'Province').
	subdivision('cd-mo', 'cd', 'Mongala', 'Province').
	subdivision('cd-nk', 'cd', 'Nord-Kivu', 'Province').
	subdivision('cd-nu', 'cd', 'Nord-Ubangi', 'Province').
	subdivision('cd-sa', 'cd', 'Sankuru', 'Province').
	subdivision('cd-sk', 'cd', 'Sud-Kivu', 'Province').
	subdivision('cd-su', 'cd', 'Sud-Ubangi', 'Province').
	subdivision('cd-ta', 'cd', 'Tanganyika', 'Province').
	subdivision('cd-to', 'cd', 'Tshopo', 'Province').
	subdivision('cd-tu', 'cd', 'Tshuapa', 'Province').
	subdivision('cf-ac', 'cf', 'Ouham', 'Prefecture').
	subdivision('cf-bb', 'cf', 'Bamingui-Bangoran', 'Prefecture').
	subdivision('cf-bgf', 'cf', 'Bangui', 'Commune').
	subdivision('cf-bk', 'cf', 'Basse-Kotto', 'Prefecture').
	subdivision('cf-hk', 'cf', 'Haute-Kotto', 'Prefecture').
	subdivision('cf-hm', 'cf', 'Haut-Mbomou', 'Prefecture').
	subdivision('cf-hs', 'cf', 'Haute-Sangha / Mambéré-Kadéï', 'Prefecture').
	subdivision('cf-kb', 'cf', 'Gribingui', 'Economic prefecture').
	subdivision('cf-kg', 'cf', 'Kémo-Gribingui', 'Prefecture').
	subdivision('cf-lb', 'cf', 'Lobaye', 'Prefecture').
	subdivision('cf-mb', 'cf', 'Mbomou', 'Prefecture').
	subdivision('cf-mp', 'cf', 'Ombella-Mpoko', 'Prefecture').
	subdivision('cf-nm', 'cf', 'Nana-Mambéré', 'Prefecture').
	subdivision('cf-op', 'cf', 'Ouham-Pendé', 'Prefecture').
	subdivision('cf-se', 'cf', 'Sangha', 'Economic prefecture').
	subdivision('cf-uk', 'cf', 'Ouaka', 'Prefecture').
	subdivision('cf-vk', 'cf', 'Vakaga', 'Prefecture').
	subdivision('cg-11', 'cg', 'Bouenza', 'Department').
	subdivision('cg-12', 'cg', 'Pool', 'Department').
	subdivision('cg-13', 'cg', 'Sangha', 'Department').
	subdivision('cg-14', 'cg', 'Plateaux', 'Department').
	subdivision('cg-15', 'cg', 'Cuvette-Ouest', 'Department').
	subdivision('cg-16', 'cg', 'Pointe-Noire', 'Department').
	subdivision('cg-2', 'cg', 'Lékoumou', 'Department').
	subdivision('cg-5', 'cg', 'Kouilou', 'Department').
	subdivision('cg-7', 'cg', 'Likouala', 'Department').
	subdivision('cg-8', 'cg', 'Cuvette', 'Department').
	subdivision('cg-9', 'cg', 'Niari', 'Department').
	subdivision('cg-bzv', 'cg', 'Brazzaville', 'Department').
	subdivision('ch-ag', 'ch', 'Aargau', 'Canton').
	subdivision('ch-ai', 'ch', 'Appenzell Innerrhoden', 'Canton').
	subdivision('ch-ar', 'ch', 'Appenzell Ausserrhoden', 'Canton').
	subdivision('ch-be', 'ch', 'Berne', 'Canton').
	subdivision('ch-bl', 'ch', 'Basel-Landschaft', 'Canton').
	subdivision('ch-bs', 'ch', 'Basel-Stadt', 'Canton').
	subdivision('ch-fr', 'ch', 'Fribourg', 'Canton').
	subdivision('ch-ge', 'ch', 'Genève', 'Canton').
	subdivision('ch-gl', 'ch', 'Glarus', 'Canton').
	subdivision('ch-gr', 'ch', 'Graubünden', 'Canton').
	subdivision('ch-ju', 'ch', 'Jura', 'Canton').
	subdivision('ch-lu', 'ch', 'Luzern', 'Canton').
	subdivision('ch-ne', 'ch', 'Neuchâtel', 'Canton').
	subdivision('ch-nw', 'ch', 'Nidwalden', 'Canton').
	subdivision('ch-ow', 'ch', 'Obwalden', 'Canton').
	subdivision('ch-sg', 'ch', 'Sankt Gallen', 'Canton').
	subdivision('ch-sh', 'ch', 'Schaffhausen', 'Canton').
	subdivision('ch-so', 'ch', 'Solothurn', 'Canton').
	subdivision('ch-sz', 'ch', 'Schwyz', 'Canton').
	subdivision('ch-tg', 'ch', 'Thurgau', 'Canton').
	subdivision('ch-ti', 'ch', 'Ticino', 'Canton').
	subdivision('ch-ur', 'ch', 'Uri', 'Canton').
	subdivision('ch-vd', 'ch', 'Vaud', 'Canton').
	subdivision('ch-vs', 'ch', 'Valais', 'Canton').
	subdivision('ch-zg', 'ch', 'Zug', 'Canton').
	subdivision('ch-zh', 'ch', 'Zürich', 'Canton').
	subdivision('ci-ab', 'ci', 'Abidjan', 'Autonomous district').
	subdivision('ci-bs', 'ci', 'Bas-Sassandra', 'District').
	subdivision('ci-cm', 'ci', 'Comoé', 'District').
	subdivision('ci-dn', 'ci', 'Denguélé', 'District').
	subdivision('ci-gd', 'ci', 'Gôh-Djiboua', 'District').
	subdivision('ci-lc', 'ci', 'Lacs', 'District').
	subdivision('ci-lg', 'ci', 'Lagunes', 'District').
	subdivision('ci-mg', 'ci', 'Montagnes', 'District').
	subdivision('ci-sm', 'ci', 'Sassandra-Marahoué', 'District').
	subdivision('ci-sv', 'ci', 'Savanes', 'District').
	subdivision('ci-vb', 'ci', 'Vallée du Bandama', 'District').
	subdivision('ci-wr', 'ci', 'Woroba', 'District').
	subdivision('ci-ym', 'ci', 'Yamoussoukro', 'Autonomous district').
	subdivision('ci-zz', 'ci', 'Zanzan', 'District').
	subdivision('cl-ai', 'cl', 'Aisén del General Carlos Ibañez del Campo', 'Region').
	subdivision('cl-an', 'cl', 'Antofagasta', 'Region').
	subdivision('cl-ap', 'cl', 'Arica y Parinacota', 'Region').
	subdivision('cl-ar', 'cl', 'La Araucanía', 'Region').
	subdivision('cl-at', 'cl', 'Atacama', 'Region').
	subdivision('cl-bi', 'cl', 'Biobío', 'Region').
	subdivision('cl-co', 'cl', 'Coquimbo', 'Region').
	subdivision('cl-li', 'cl', 'Libertador General Bernardo O''Higgins', 'Region').
	subdivision('cl-ll', 'cl', 'Los Lagos', 'Region').
	subdivision('cl-lr', 'cl', 'Los Ríos', 'Region').
	subdivision('cl-ma', 'cl', 'Magallanes', 'Region').
	subdivision('cl-ml', 'cl', 'Maule', 'Region').
	subdivision('cl-nb', 'cl', 'Ñuble', 'Region').
	subdivision('cl-rm', 'cl', 'Región Metropolitana de Santiago', 'Region').
	subdivision('cl-ta', 'cl', 'Tarapacá', 'Region').
	subdivision('cl-vs', 'cl', 'Valparaíso', 'Region').
	subdivision('cm-ad', 'cm', 'Adamaoua', 'Region').
	subdivision('cm-ce', 'cm', 'Centre', 'Region').
	subdivision('cm-en', 'cm', 'Far North', 'Region').
	subdivision('cm-es', 'cm', 'East', 'Region').
	subdivision('cm-lt', 'cm', 'Littoral', 'Region').
	subdivision('cm-no', 'cm', 'North', 'Region').
	subdivision('cm-nw', 'cm', 'North-West', 'Region').
	subdivision('cm-ou', 'cm', 'West', 'Region').
	subdivision('cm-su', 'cm', 'South', 'Region').
	subdivision('cm-sw', 'cm', 'South-West', 'Region').
	subdivision('cn-ah', 'cn', 'Anhui Sheng', 'Province').
	subdivision('cn-bj', 'cn', 'Beijing Shi', 'Municipality').
	subdivision('cn-cq', 'cn', 'Chongqing Shi', 'Municipality').
	subdivision('cn-fj', 'cn', 'Fujian Sheng', 'Province').
	subdivision('cn-gd', 'cn', 'Guangdong Sheng', 'Province').
	subdivision('cn-gs', 'cn', 'Gansu Sheng', 'Province').
	subdivision('cn-gx', 'cn', 'Guangxi Zhuangzu Zizhiqu', 'Autonomous region').
	subdivision('cn-gz', 'cn', 'Guizhou Sheng', 'Province').
	subdivision('cn-ha', 'cn', 'Henan Sheng', 'Province').
	subdivision('cn-hb', 'cn', 'Hubei Sheng', 'Province').
	subdivision('cn-he', 'cn', 'Hebei Sheng', 'Province').
	subdivision('cn-hi', 'cn', 'Hainan Sheng', 'Province').
	subdivision('cn-hk', 'cn', 'Hong Kong SAR', 'Special administrative region').
	subdivision('cn-hl', 'cn', 'Heilongjiang Sheng', 'Province').
	subdivision('cn-hn', 'cn', 'Hunan Sheng', 'Province').
	subdivision('cn-jl', 'cn', 'Jilin Sheng', 'Province').
	subdivision('cn-js', 'cn', 'Jiangsu Sheng', 'Province').
	subdivision('cn-jx', 'cn', 'Jiangxi Sheng', 'Province').
	subdivision('cn-ln', 'cn', 'Liaoning Sheng', 'Province').
	subdivision('cn-mo', 'cn', 'Macao SAR', 'Special administrative region').
	subdivision('cn-nm', 'cn', 'Nei Mongol Zizhiqu', 'Autonomous region').
	subdivision('cn-nx', 'cn', 'Ningxia Huizu Zizhiqu', 'Autonomous region').
	subdivision('cn-qh', 'cn', 'Qinghai Sheng', 'Province').
	subdivision('cn-sc', 'cn', 'Sichuan Sheng', 'Province').
	subdivision('cn-sd', 'cn', 'Shandong Sheng', 'Province').
	subdivision('cn-sh', 'cn', 'Shanghai Shi', 'Municipality').
	subdivision('cn-sn', 'cn', 'Shaanxi Sheng', 'Province').
	subdivision('cn-sx', 'cn', 'Shanxi Sheng', 'Province').
	subdivision('cn-tj', 'cn', 'Tianjin Shi', 'Municipality').
	subdivision('cn-tw', 'cn', 'Taiwan Sheng', 'Province').
	subdivision('cn-xj', 'cn', 'Xinjiang Uygur Zizhiqu', 'Autonomous region').
	subdivision('cn-xz', 'cn', 'Xizang Zizhiqu', 'Autonomous region').
	subdivision('cn-yn', 'cn', 'Yunnan Sheng', 'Province').
	subdivision('cn-zj', 'cn', 'Zhejiang Sheng', 'Province').
	subdivision('co-ama', 'co', 'Amazonas', 'Department').
	subdivision('co-ant', 'co', 'Antioquia', 'Department').
	subdivision('co-ara', 'co', 'Arauca', 'Department').
	subdivision('co-atl', 'co', 'Atlántico', 'Department').
	subdivision('co-bol', 'co', 'Bolívar', 'Department').
	subdivision('co-boy', 'co', 'Boyacá', 'Department').
	subdivision('co-cal', 'co', 'Caldas', 'Department').
	subdivision('co-caq', 'co', 'Caquetá', 'Department').
	subdivision('co-cas', 'co', 'Casanare', 'Department').
	subdivision('co-cau', 'co', 'Cauca', 'Department').
	subdivision('co-ces', 'co', 'Cesar', 'Department').
	subdivision('co-cho', 'co', 'Chocó', 'Department').
	subdivision('co-cor', 'co', 'Córdoba', 'Department').
	subdivision('co-cun', 'co', 'Cundinamarca', 'Department').
	subdivision('co-dc', 'co', 'Distrito Capital de Bogotá', 'Capital district').
	subdivision('co-gua', 'co', 'Guainía', 'Department').
	subdivision('co-guv', 'co', 'Guaviare', 'Department').
	subdivision('co-hui', 'co', 'Huila', 'Department').
	subdivision('co-lag', 'co', 'La Guajira', 'Department').
	subdivision('co-mag', 'co', 'Magdalena', 'Department').
	subdivision('co-met', 'co', 'Meta', 'Department').
	subdivision('co-nar', 'co', 'Nariño', 'Department').
	subdivision('co-nsa', 'co', 'Norte de Santander', 'Department').
	subdivision('co-put', 'co', 'Putumayo', 'Department').
	subdivision('co-qui', 'co', 'Quindío', 'Department').
	subdivision('co-ris', 'co', 'Risaralda', 'Department').
	subdivision('co-san', 'co', 'Santander', 'Department').
	subdivision('co-sap', 'co', 'San Andrés, Providencia y Santa Catalina', 'Department').
	subdivision('co-suc', 'co', 'Sucre', 'Department').
	subdivision('co-tol', 'co', 'Tolima', 'Department').
	subdivision('co-vac', 'co', 'Valle del Cauca', 'Department').
	subdivision('co-vau', 'co', 'Vaupés', 'Department').
	subdivision('co-vid', 'co', 'Vichada', 'Department').
	subdivision('cr-a', 'cr', 'Alajuela', 'Province').
	subdivision('cr-c', 'cr', 'Cartago', 'Province').
	subdivision('cr-g', 'cr', 'Guanacaste', 'Province').
	subdivision('cr-h', 'cr', 'Heredia', 'Province').
	subdivision('cr-l', 'cr', 'Limón', 'Province').
	subdivision('cr-p', 'cr', 'Puntarenas', 'Province').
	subdivision('cr-sj', 'cr', 'San José', 'Province').
	subdivision('cu-01', 'cu', 'Pinar del Río', 'Province').
	subdivision('cu-03', 'cu', 'La Habana', 'Province').
	subdivision('cu-04', 'cu', 'Matanzas', 'Province').
	subdivision('cu-05', 'cu', 'Villa Clara', 'Province').
	subdivision('cu-06', 'cu', 'Cienfuegos', 'Province').
	subdivision('cu-07', 'cu', 'Sancti Spíritus', 'Province').
	subdivision('cu-08', 'cu', 'Ciego de Ávila', 'Province').
	subdivision('cu-09', 'cu', 'Camagüey', 'Province').
	subdivision('cu-10', 'cu', 'Las Tunas', 'Province').
	subdivision('cu-11', 'cu', 'Holguín', 'Province').
	subdivision('cu-12', 'cu', 'Granma', 'Province').
	subdivision('cu-13', 'cu', 'Santiago de Cuba', 'Province').
	subdivision('cu-14', 'cu', 'Guantánamo', 'Province').
	subdivision('cu-15', 'cu', 'Artemisa', 'Province').
	subdivision('cu-16', 'cu', 'Mayabeque', 'Province').
	subdivision('cu-99', 'cu', 'Isla de la Juventud', 'Special municipality').
	subdivision('cv-b', 'cv', 'Ilhas de Barlavento', 'Geographical region').
	subdivision('cv-br', 'cv', 'Brava', 'Municipality').
	subdivision('cv-bv', 'cv', 'Boa Vista', 'Municipality').
	subdivision('cv-ca', 'cv', 'Santa Catarina', 'Municipality').
	subdivision('cv-cf', 'cv', 'Santa Catarina do Fogo', 'Municipality').
	subdivision('cv-cr', 'cv', 'Santa Cruz', 'Municipality').
	subdivision('cv-ma', 'cv', 'Maio', 'Municipality').
	subdivision('cv-mo', 'cv', 'Mosteiros', 'Municipality').
	subdivision('cv-pa', 'cv', 'Paul', 'Municipality').
	subdivision('cv-pn', 'cv', 'Porto Novo', 'Municipality').
	subdivision('cv-pr', 'cv', 'Praia', 'Municipality').
	subdivision('cv-rb', 'cv', 'Ribeira Brava', 'Municipality').
	subdivision('cv-rg', 'cv', 'Ribeira Grande', 'Municipality').
	subdivision('cv-rs', 'cv', 'Ribeira Grande de Santiago', 'Municipality').
	subdivision('cv-s', 'cv', 'Ilhas de Sotavento', 'Geographical region').
	subdivision('cv-sd', 'cv', 'São Domingos', 'Municipality').
	subdivision('cv-sf', 'cv', 'São Filipe', 'Municipality').
	subdivision('cv-sl', 'cv', 'Sal', 'Municipality').
	subdivision('cv-sm', 'cv', 'São Miguel', 'Municipality').
	subdivision('cv-so', 'cv', 'São Lourenço dos Órgãos', 'Municipality').
	subdivision('cv-ss', 'cv', 'São Salvador do Mundo', 'Municipality').
	subdivision('cv-sv', 'cv', 'São Vicente', 'Municipality').
	subdivision('cv-ta', 'cv', 'Tarrafal', 'Municipality').
	subdivision('cv-ts', 'cv', 'Tarrafal de São Nicolau', 'Municipality').
	subdivision('cy-01', 'cy', 'Lefkosia', 'District').
	subdivision('cy-02', 'cy', 'Lemesos', 'District').
	subdivision('cy-03', 'cy', 'Larnaka', 'District').
	subdivision('cy-04', 'cy', 'Ammochostos', 'District').
	subdivision('cy-05', 'cy', 'Pafos', 'District').
	subdivision('cy-06', 'cy', 'Keryneia', 'District').
	subdivision('cz-10', 'cz', 'Praha, Hlavní město', 'Capital city').
	subdivision('cz-20', 'cz', 'Středočeský kraj', 'Region').
	subdivision('cz-201', 'cz', 'Benešov', 'District').
	subdivision('cz-202', 'cz', 'Beroun', 'District').
	subdivision('cz-203', 'cz', 'Kladno', 'District').
	subdivision('cz-204', 'cz', 'Kolín', 'District').
	subdivision('cz-205', 'cz', 'Kutná Hora', 'District').
	subdivision('cz-206', 'cz', 'Mělník', 'District').
	subdivision('cz-207', 'cz', 'Mladá Boleslav', 'District').
	subdivision('cz-208', 'cz', 'Nymburk', 'District').
	subdivision('cz-209', 'cz', 'Praha-východ', 'District').
	subdivision('cz-20a', 'cz', 'Praha-západ', 'District').
	subdivision('cz-20b', 'cz', 'Příbram', 'District').
	subdivision('cz-20c', 'cz', 'Rakovník', 'District').
	subdivision('cz-31', 'cz', 'Jihočeský kraj', 'Region').
	subdivision('cz-311', 'cz', 'České Budějovice', 'District').
	subdivision('cz-312', 'cz', 'Český Krumlov', 'District').
	subdivision('cz-313', 'cz', 'Jindřichův Hradec', 'District').
	subdivision('cz-314', 'cz', 'Písek', 'District').
	subdivision('cz-315', 'cz', 'Prachatice', 'District').
	subdivision('cz-316', 'cz', 'Strakonice', 'District').
	subdivision('cz-317', 'cz', 'Tábor', 'District').
	subdivision('cz-32', 'cz', 'Plzeňský kraj', 'Region').
	subdivision('cz-321', 'cz', 'Domažlice', 'District').
	subdivision('cz-322', 'cz', 'Klatovy', 'District').
	subdivision('cz-323', 'cz', 'Plzeň-město', 'District').
	subdivision('cz-324', 'cz', 'Plzeň-jih', 'District').
	subdivision('cz-325', 'cz', 'Plzeň-sever', 'District').
	subdivision('cz-326', 'cz', 'Rokycany', 'District').
	subdivision('cz-327', 'cz', 'Tachov', 'District').
	subdivision('cz-41', 'cz', 'Karlovarský kraj', 'Region').
	subdivision('cz-411', 'cz', 'Cheb', 'District').
	subdivision('cz-412', 'cz', 'Karlovy Vary', 'District').
	subdivision('cz-413', 'cz', 'Sokolov', 'District').
	subdivision('cz-42', 'cz', 'Ústecký kraj', 'Region').
	subdivision('cz-421', 'cz', 'Děčín', 'District').
	subdivision('cz-422', 'cz', 'Chomutov', 'District').
	subdivision('cz-423', 'cz', 'Litoměřice', 'District').
	subdivision('cz-424', 'cz', 'Louny', 'District').
	subdivision('cz-425', 'cz', 'Most', 'District').
	subdivision('cz-426', 'cz', 'Teplice', 'District').
	subdivision('cz-427', 'cz', 'Ústí nad Labem', 'District').
	subdivision('cz-51', 'cz', 'Liberecký kraj', 'Region').
	subdivision('cz-511', 'cz', 'Česká Lípa', 'District').
	subdivision('cz-512', 'cz', 'Jablonec nad Nisou', 'District').
	subdivision('cz-513', 'cz', 'Liberec', 'District').
	subdivision('cz-514', 'cz', 'Semily', 'District').
	subdivision('cz-52', 'cz', 'Královéhradecký kraj', 'Region').
	subdivision('cz-521', 'cz', 'Hradec Králové', 'District').
	subdivision('cz-522', 'cz', 'Jičín', 'District').
	subdivision('cz-523', 'cz', 'Náchod', 'District').
	subdivision('cz-524', 'cz', 'Rychnov nad Kněžnou', 'District').
	subdivision('cz-525', 'cz', 'Trutnov', 'District').
	subdivision('cz-53', 'cz', 'Pardubický kraj', 'Region').
	subdivision('cz-531', 'cz', 'Chrudim', 'District').
	subdivision('cz-532', 'cz', 'Pardubice', 'District').
	subdivision('cz-533', 'cz', 'Svitavy', 'District').
	subdivision('cz-534', 'cz', 'Ústí nad Orlicí', 'District').
	subdivision('cz-63', 'cz', 'Kraj Vysočina', 'Region').
	subdivision('cz-631', 'cz', 'Havlíčkův Brod', 'District').
	subdivision('cz-632', 'cz', 'Jihlava', 'District').
	subdivision('cz-633', 'cz', 'Pelhřimov', 'District').
	subdivision('cz-634', 'cz', 'Třebíč', 'District').
	subdivision('cz-635', 'cz', 'Žďár nad Sázavou', 'District').
	subdivision('cz-64', 'cz', 'Jihomoravský kraj', 'Region').
	subdivision('cz-641', 'cz', 'Blansko', 'District').
	subdivision('cz-642', 'cz', 'Brno-město', 'District').
	subdivision('cz-643', 'cz', 'Brno-venkov', 'District').
	subdivision('cz-644', 'cz', 'Břeclav', 'District').
	subdivision('cz-645', 'cz', 'Hodonín', 'District').
	subdivision('cz-646', 'cz', 'Vyškov', 'District').
	subdivision('cz-647', 'cz', 'Znojmo', 'District').
	subdivision('cz-71', 'cz', 'Olomoucký kraj', 'Region').
	subdivision('cz-711', 'cz', 'Jeseník', 'District').
	subdivision('cz-712', 'cz', 'Olomouc', 'District').
	subdivision('cz-713', 'cz', 'Prostějov', 'District').
	subdivision('cz-714', 'cz', 'Přerov', 'District').
	subdivision('cz-715', 'cz', 'Šumperk', 'District').
	subdivision('cz-72', 'cz', 'Zlínský kraj', 'Region').
	subdivision('cz-721', 'cz', 'Kroměříž', 'District').
	subdivision('cz-722', 'cz', 'Uherské Hradiště', 'District').
	subdivision('cz-723', 'cz', 'Vsetín', 'District').
	subdivision('cz-724', 'cz', 'Zlín', 'District').
	subdivision('cz-80', 'cz', 'Moravskoslezský kraj', 'Region').
	subdivision('cz-801', 'cz', 'Bruntál', 'District').
	subdivision('cz-802', 'cz', 'Frýdek-Místek', 'District').
	subdivision('cz-803', 'cz', 'Karviná', 'District').
	subdivision('cz-804', 'cz', 'Nový Jičín', 'District').
	subdivision('cz-805', 'cz', 'Opava', 'District').
	subdivision('cz-806', 'cz', 'Ostrava-město', 'District').
	subdivision('de-bb', 'de', 'Brandenburg', 'Land').
	subdivision('de-be', 'de', 'Berlin', 'Land').
	subdivision('de-bw', 'de', 'Baden-Württemberg', 'Land').
	subdivision('de-by', 'de', 'Bayern', 'Land').
	subdivision('de-hb', 'de', 'Bremen', 'Land').
	subdivision('de-he', 'de', 'Hessen', 'Land').
	subdivision('de-hh', 'de', 'Hamburg', 'Land').
	subdivision('de-mv', 'de', 'Mecklenburg-Vorpommern', 'Land').
	subdivision('de-ni', 'de', 'Niedersachsen', 'Land').
	subdivision('de-nw', 'de', 'Nordrhein-Westfalen', 'Land').
	subdivision('de-rp', 'de', 'Rheinland-Pfalz', 'Land').
	subdivision('de-sh', 'de', 'Schleswig-Holstein', 'Land').
	subdivision('de-sl', 'de', 'Saarland', 'Land').
	subdivision('de-sn', 'de', 'Sachsen', 'Land').
	subdivision('de-st', 'de', 'Sachsen-Anhalt', 'Land').
	subdivision('de-th', 'de', 'Thüringen', 'Land').
	subdivision('dj-ar', 'dj', 'Arta', 'Region').
	subdivision('dj-as', 'dj', 'Ali Sabieh', 'Region').
	subdivision('dj-di', 'dj', 'Dikhil', 'Region').
	subdivision('dj-dj', 'dj', 'Djibouti', 'City').
	subdivision('dj-ob', 'dj', 'Obock', 'Region').
	subdivision('dj-ta', 'dj', 'Tadjourah', 'Region').
	subdivision('dk-81', 'dk', 'Nordjylland', 'Region').
	subdivision('dk-82', 'dk', 'Midtjylland', 'Region').
	subdivision('dk-83', 'dk', 'Syddanmark', 'Region').
	subdivision('dk-84', 'dk', 'Hovedstaden', 'Region').
	subdivision('dk-85', 'dk', 'Sjælland', 'Region').
	subdivision('dm-02', 'dm', 'Saint Andrew', 'Parish').
	subdivision('dm-03', 'dm', 'Saint David', 'Parish').
	subdivision('dm-04', 'dm', 'Saint George', 'Parish').
	subdivision('dm-05', 'dm', 'Saint John', 'Parish').
	subdivision('dm-06', 'dm', 'Saint Joseph', 'Parish').
	subdivision('dm-07', 'dm', 'Saint Luke', 'Parish').
	subdivision('dm-08', 'dm', 'Saint Mark', 'Parish').
	subdivision('dm-09', 'dm', 'Saint Patrick', 'Parish').
	subdivision('dm-10', 'dm', 'Saint Paul', 'Parish').
	subdivision('dm-11', 'dm', 'Saint Peter', 'Parish').
	subdivision('do-01', 'do', 'Distrito Nacional (Santo Domingo)', 'District').
	subdivision('do-02', 'do', 'Azua', 'Province').
	subdivision('do-03', 'do', 'Baoruco', 'Province').
	subdivision('do-04', 'do', 'Barahona', 'Province').
	subdivision('do-05', 'do', 'Dajabón', 'Province').
	subdivision('do-06', 'do', 'Duarte', 'Province').
	subdivision('do-07', 'do', 'Elías Piña', 'Province').
	subdivision('do-08', 'do', 'El Seibo', 'Province').
	subdivision('do-09', 'do', 'Espaillat', 'Province').
	subdivision('do-10', 'do', 'Independencia', 'Province').
	subdivision('do-11', 'do', 'La Altagracia', 'Province').
	subdivision('do-12', 'do', 'La Romana', 'Province').
	subdivision('do-13', 'do', 'La Vega', 'Province').
	subdivision('do-14', 'do', 'María Trinidad Sánchez', 'Province').
	subdivision('do-15', 'do', 'Monte Cristi', 'Province').
	subdivision('do-16', 'do', 'Pedernales', 'Province').
	subdivision('do-17', 'do', 'Peravia', 'Province').
	subdivision('do-18', 'do', 'Puerto Plata', 'Province').
	subdivision('do-19', 'do', 'Hermanas Mirabal', 'Province').
	subdivision('do-20', 'do', 'Samaná', 'Province').
	subdivision('do-21', 'do', 'San Cristóbal', 'Province').
	subdivision('do-22', 'do', 'San Juan', 'Province').
	subdivision('do-23', 'do', 'San Pedro de Macorís', 'Province').
	subdivision('do-24', 'do', 'Sánchez Ramírez', 'Province').
	subdivision('do-25', 'do', 'Santiago', 'Province').
	subdivision('do-26', 'do', 'Santiago Rodríguez', 'Province').
	subdivision('do-27', 'do', 'Valverde', 'Province').
	subdivision('do-28', 'do', 'Monseñor Nouel', 'Province').
	subdivision('do-29', 'do', 'Monte Plata', 'Province').
	subdivision('do-30', 'do', 'Hato Mayor', 'Province').
	subdivision('do-31', 'do', 'San José de Ocoa', 'Province').
	subdivision('do-32', 'do', 'Santo Domingo', 'Province').
	subdivision('do-33', 'do', 'Cibao Nordeste', 'Region').
	subdivision('do-34', 'do', 'Cibao Noroeste', 'Region').
	subdivision('do-35', 'do', 'Cibao Norte', 'Region').
	subdivision('do-36', 'do', 'Cibao Sur', 'Region').
	subdivision('do-37', 'do', 'El Valle', 'Region').
	subdivision('do-38', 'do', 'Enriquillo', 'Region').
	subdivision('do-39', 'do', 'Higuamo', 'Region').
	subdivision('do-40', 'do', 'Ozama', 'Region').
	subdivision('do-41', 'do', 'Valdesia', 'Region').
	subdivision('do-42', 'do', 'Yuma', 'Region').
	subdivision('dz-01', 'dz', 'Adrar', 'Province').
	subdivision('dz-02', 'dz', 'Chlef', 'Province').
	subdivision('dz-03', 'dz', 'Laghouat', 'Province').
	subdivision('dz-04', 'dz', 'Oum el Bouaghi', 'Province').
	subdivision('dz-05', 'dz', 'Batna', 'Province').
	subdivision('dz-06', 'dz', 'Béjaïa', 'Province').
	subdivision('dz-07', 'dz', 'Biskra', 'Province').
	subdivision('dz-08', 'dz', 'Béchar', 'Province').
	subdivision('dz-09', 'dz', 'Blida', 'Province').
	subdivision('dz-10', 'dz', 'Bouira', 'Province').
	subdivision('dz-11', 'dz', 'Tamanrasset', 'Province').
	subdivision('dz-12', 'dz', 'Tébessa', 'Province').
	subdivision('dz-13', 'dz', 'Tlemcen', 'Province').
	subdivision('dz-14', 'dz', 'Tiaret', 'Province').
	subdivision('dz-15', 'dz', 'Tizi Ouzou', 'Province').
	subdivision('dz-16', 'dz', 'Alger', 'Province').
	subdivision('dz-17', 'dz', 'Djelfa', 'Province').
	subdivision('dz-18', 'dz', 'Jijel', 'Province').
	subdivision('dz-19', 'dz', 'Sétif', 'Province').
	subdivision('dz-20', 'dz', 'Saïda', 'Province').
	subdivision('dz-21', 'dz', 'Skikda', 'Province').
	subdivision('dz-22', 'dz', 'Sidi Bel Abbès', 'Province').
	subdivision('dz-23', 'dz', 'Annaba', 'Province').
	subdivision('dz-24', 'dz', 'Guelma', 'Province').
	subdivision('dz-25', 'dz', 'Constantine', 'Province').
	subdivision('dz-26', 'dz', 'Médéa', 'Province').
	subdivision('dz-27', 'dz', 'Mostaganem', 'Province').
	subdivision('dz-28', 'dz', 'M''sila', 'Province').
	subdivision('dz-29', 'dz', 'Mascara', 'Province').
	subdivision('dz-30', 'dz', 'Ouargla', 'Province').
	subdivision('dz-31', 'dz', 'Oran', 'Province').
	subdivision('dz-32', 'dz', 'El Bayadh', 'Province').
	subdivision('dz-33', 'dz', 'Illizi', 'Province').
	subdivision('dz-34', 'dz', 'Bordj Bou Arréridj', 'Province').
	subdivision('dz-35', 'dz', 'Boumerdès', 'Province').
	subdivision('dz-36', 'dz', 'El Tarf', 'Province').
	subdivision('dz-37', 'dz', 'Tindouf', 'Province').
	subdivision('dz-38', 'dz', 'Tissemsilt', 'Province').
	subdivision('dz-39', 'dz', 'El Oued', 'Province').
	subdivision('dz-40', 'dz', 'Khenchela', 'Province').
	subdivision('dz-41', 'dz', 'Souk Ahras', 'Province').
	subdivision('dz-42', 'dz', 'Tipaza', 'Province').
	subdivision('dz-43', 'dz', 'Mila', 'Province').
	subdivision('dz-44', 'dz', 'Aïn Defla', 'Province').
	subdivision('dz-45', 'dz', 'Naama', 'Province').
	subdivision('dz-46', 'dz', 'Aïn Témouchent', 'Province').
	subdivision('dz-47', 'dz', 'Ghardaïa', 'Province').
	subdivision('dz-48', 'dz', 'Relizane', 'Province').
	subdivision('dz-49', 'dz', 'Timimoun', 'Province').
	subdivision('dz-50', 'dz', 'Bordj Badji Mokhtar', 'Province').
	subdivision('dz-51', 'dz', 'Ouled Djellal', 'Province').
	subdivision('dz-52', 'dz', 'Béni Abbès', 'Province').
	subdivision('dz-53', 'dz', 'In Salah', 'Province').
	subdivision('dz-54', 'dz', 'In Guezzam', 'Province').
	subdivision('dz-55', 'dz', 'Touggourt', 'Province').
	subdivision('dz-56', 'dz', 'Djanet', 'Province').
	subdivision('dz-57', 'dz', 'El Meghaier', 'Province').
	subdivision('dz-58', 'dz', 'El Meniaa', 'Province').
	subdivision('ec-a', 'ec', 'Azuay', 'Province').
	subdivision('ec-b', 'ec', 'Bolívar', 'Province').
	subdivision('ec-c', 'ec', 'Carchi', 'Province').
	subdivision('ec-d', 'ec', 'Orellana', 'Province').
	subdivision('ec-e', 'ec', 'Esmeraldas', 'Province').
	subdivision('ec-f', 'ec', 'Cañar', 'Province').
	subdivision('ec-g', 'ec', 'Guayas', 'Province').
	subdivision('ec-h', 'ec', 'Chimborazo', 'Province').
	subdivision('ec-i', 'ec', 'Imbabura', 'Province').
	subdivision('ec-l', 'ec', 'Loja', 'Province').
	subdivision('ec-m', 'ec', 'Manabí', 'Province').
	subdivision('ec-n', 'ec', 'Napo', 'Province').
	subdivision('ec-o', 'ec', 'El Oro', 'Province').
	subdivision('ec-p', 'ec', 'Pichincha', 'Province').
	subdivision('ec-r', 'ec', 'Los Ríos', 'Province').
	subdivision('ec-s', 'ec', 'Morona Santiago', 'Province').
	subdivision('ec-sd', 'ec', 'Santo Domingo de los Tsáchilas', 'Province').
	subdivision('ec-se', 'ec', 'Santa Elena', 'Province').
	subdivision('ec-t', 'ec', 'Tungurahua', 'Province').
	subdivision('ec-u', 'ec', 'Sucumbíos', 'Province').
	subdivision('ec-w', 'ec', 'Galápagos', 'Province').
	subdivision('ec-x', 'ec', 'Cotopaxi', 'Province').
	subdivision('ec-y', 'ec', 'Pastaza', 'Province').
	subdivision('ec-z', 'ec', 'Zamora Chinchipe', 'Province').
	subdivision('ee-130', 'ee', 'Alutaguse', 'Rural municipality').
	subdivision('ee-141', 'ee', 'Anija', 'Rural municipality').
	subdivision('ee-142', 'ee', 'Antsla', 'Rural municipality').
	subdivision('ee-171', 'ee', 'Elva', 'Rural municipality').
	subdivision('ee-184', 'ee', 'Haapsalu', 'Urban municipality').
	subdivision('ee-191', 'ee', 'Haljala', 'Rural municipality').
	subdivision('ee-198', 'ee', 'Harku', 'Rural municipality').
	subdivision('ee-205', 'ee', 'Hiiumaa', 'Rural municipality').
	subdivision('ee-214', 'ee', 'Häädemeeste', 'Rural municipality').
	subdivision('ee-245', 'ee', 'Jõelähtme', 'Rural municipality').
	subdivision('ee-247', 'ee', 'Jõgeva', 'Rural municipality').
	subdivision('ee-251', 'ee', 'Jõhvi', 'Rural municipality').
	subdivision('ee-255', 'ee', 'Järva', 'Rural municipality').
	subdivision('ee-272', 'ee', 'Kadrina', 'Rural municipality').
	subdivision('ee-283', 'ee', 'Kambja', 'Rural municipality').
	subdivision('ee-284', 'ee', 'Kanepi', 'Rural municipality').
	subdivision('ee-291', 'ee', 'Kastre', 'Rural municipality').
	subdivision('ee-293', 'ee', 'Kehtna', 'Rural municipality').
	subdivision('ee-296', 'ee', 'Keila', 'Urban municipality').
	subdivision('ee-303', 'ee', 'Kihnu', 'Rural municipality').
	subdivision('ee-305', 'ee', 'Kiili', 'Rural municipality').
	subdivision('ee-317', 'ee', 'Kohila', 'Rural municipality').
	subdivision('ee-321', 'ee', 'Kohtla-Järve', 'Urban municipality').
	subdivision('ee-338', 'ee', 'Kose', 'Rural municipality').
	subdivision('ee-353', 'ee', 'Kuusalu', 'Rural municipality').
	subdivision('ee-37', 'ee', 'Harjumaa', 'County').
	subdivision('ee-39', 'ee', 'Hiiumaa', 'County').
	subdivision('ee-424', 'ee', 'Loksa', 'Urban municipality').
	subdivision('ee-430', 'ee', 'Lääneranna', 'Rural municipality').
	subdivision('ee-431', 'ee', 'Lääne-Harju', 'Rural municipality').
	subdivision('ee-432', 'ee', 'Luunja', 'Rural municipality').
	subdivision('ee-441', 'ee', 'Lääne-Nigula', 'Rural municipality').
	subdivision('ee-442', 'ee', 'Lüganuse', 'Rural municipality').
	subdivision('ee-446', 'ee', 'Maardu', 'Urban municipality').
	subdivision('ee-45', 'ee', 'Ida-Virumaa', 'County').
	subdivision('ee-478', 'ee', 'Muhu', 'Rural municipality').
	subdivision('ee-480', 'ee', 'Mulgi', 'Rural municipality').
	subdivision('ee-486', 'ee', 'Mustvee', 'Rural municipality').
	subdivision('ee-50', 'ee', 'Jõgevamaa', 'County').
	subdivision('ee-503', 'ee', 'Märjamaa', 'Rural municipality').
	subdivision('ee-511', 'ee', 'Narva', 'Urban municipality').
	subdivision('ee-514', 'ee', 'Narva-Jõesuu', 'Urban municipality').
	subdivision('ee-52', 'ee', 'Järvamaa', 'County').
	subdivision('ee-528', 'ee', 'Nõo', 'Rural municipality').
	subdivision('ee-557', 'ee', 'Otepää', 'Rural municipality').
	subdivision('ee-56', 'ee', 'Läänemaa', 'County').
	subdivision('ee-567', 'ee', 'Paide', 'Urban municipality').
	subdivision('ee-586', 'ee', 'Peipsiääre', 'Rural municipality').
	subdivision('ee-60', 'ee', 'Lääne-Virumaa', 'County').
	subdivision('ee-615', 'ee', 'Põhja-Sakala', 'Rural municipality').
	subdivision('ee-618', 'ee', 'Põltsamaa', 'Rural municipality').
	subdivision('ee-622', 'ee', 'Põlva', 'Rural municipality').
	subdivision('ee-624', 'ee', 'Pärnu', 'Urban municipality').
	subdivision('ee-638', 'ee', 'Põhja-Pärnumaa', 'Rural municipality').
	subdivision('ee-64', 'ee', 'Põlvamaa', 'County').
	subdivision('ee-651', 'ee', 'Raasiku', 'Rural municipality').
	subdivision('ee-653', 'ee', 'Rae', 'Rural municipality').
	subdivision('ee-661', 'ee', 'Rakvere', 'Rural municipality').
	subdivision('ee-663', 'ee', 'Rakvere', 'Urban municipality').
	subdivision('ee-668', 'ee', 'Rapla', 'Rural municipality').
	subdivision('ee-68', 'ee', 'Pärnumaa', 'County').
	subdivision('ee-689', 'ee', 'Ruhnu', 'Rural municipality').
	subdivision('ee-698', 'ee', 'Rõuge', 'Rural municipality').
	subdivision('ee-708', 'ee', 'Räpina', 'Rural municipality').
	subdivision('ee-71', 'ee', 'Raplamaa', 'County').
	subdivision('ee-712', 'ee', 'Saarde', 'Rural municipality').
	subdivision('ee-714', 'ee', 'Saaremaa', 'Rural municipality').
	subdivision('ee-719', 'ee', 'Saku', 'Rural municipality').
	subdivision('ee-726', 'ee', 'Saue', 'Rural municipality').
	subdivision('ee-732', 'ee', 'Setomaa', 'Rural municipality').
	subdivision('ee-735', 'ee', 'Sillamäe', 'Urban municipality').
	subdivision('ee-74', 'ee', 'Saaremaa', 'County').
	subdivision('ee-784', 'ee', 'Tallinn', 'Urban municipality').
	subdivision('ee-79', 'ee', 'Tartumaa', 'County').
	subdivision('ee-792', 'ee', 'Tapa', 'Rural municipality').
	subdivision('ee-793', 'ee', 'Tartu', 'Urban municipality').
	subdivision('ee-796', 'ee', 'Tartu', 'Rural municipality').
	subdivision('ee-803', 'ee', 'Toila', 'Rural municipality').
	subdivision('ee-809', 'ee', 'Tori', 'Rural municipality').
	subdivision('ee-81', 'ee', 'Valgamaa', 'County').
	subdivision('ee-824', 'ee', 'Tõrva', 'Rural municipality').
	subdivision('ee-834', 'ee', 'Türi', 'Rural municipality').
	subdivision('ee-84', 'ee', 'Viljandimaa', 'County').
	subdivision('ee-855', 'ee', 'Valga', 'Rural municipality').
	subdivision('ee-87', 'ee', 'Võrumaa', 'County').
	subdivision('ee-890', 'ee', 'Viimsi', 'Rural municipality').
	subdivision('ee-897', 'ee', 'Viljandi', 'Urban municipality').
	subdivision('ee-899', 'ee', 'Viljandi', 'Rural municipality').
	subdivision('ee-901', 'ee', 'Vinni', 'Rural municipality').
	subdivision('ee-903', 'ee', 'Viru-Nigula', 'Rural municipality').
	subdivision('ee-907', 'ee', 'Vormsi', 'Rural municipality').
	subdivision('ee-917', 'ee', 'Võru', 'Rural municipality').
	subdivision('ee-919', 'ee', 'Võru', 'Urban municipality').
	subdivision('ee-928', 'ee', 'Väike-Maarja', 'Rural municipality').
	subdivision('eg-alx', 'eg', 'Al Iskandarīyah', 'Governorate').
	subdivision('eg-asn', 'eg', 'Aswān', 'Governorate').
	subdivision('eg-ast', 'eg', 'Asyūţ', 'Governorate').
	subdivision('eg-ba', 'eg', 'Al Baḩr al Aḩmar', 'Governorate').
	subdivision('eg-bh', 'eg', 'Al Buḩayrah', 'Governorate').
	subdivision('eg-bns', 'eg', 'Banī Suwayf', 'Governorate').
	subdivision('eg-c', 'eg', 'Al Qāhirah', 'Governorate').
	subdivision('eg-dk', 'eg', 'Ad Daqahlīyah', 'Governorate').
	subdivision('eg-dt', 'eg', 'Dumyāţ', 'Governorate').
	subdivision('eg-fym', 'eg', 'Al Fayyūm', 'Governorate').
	subdivision('eg-gh', 'eg', 'Al Gharbīyah', 'Governorate').
	subdivision('eg-gz', 'eg', 'Al Jīzah', 'Governorate').
	subdivision('eg-is', 'eg', 'Al Ismā''īlīyah', 'Governorate').
	subdivision('eg-js', 'eg', 'Janūb Sīnā''', 'Governorate').
	subdivision('eg-kb', 'eg', 'Al Qalyūbīyah', 'Governorate').
	subdivision('eg-kfs', 'eg', 'Kafr ash Shaykh', 'Governorate').
	subdivision('eg-kn', 'eg', 'Qinā', 'Governorate').
	subdivision('eg-lx', 'eg', 'Al Uqşur', 'Governorate').
	subdivision('eg-mn', 'eg', 'Al Minyā', 'Governorate').
	subdivision('eg-mnf', 'eg', 'Al Minūfīyah', 'Governorate').
	subdivision('eg-mt', 'eg', 'Maţrūḩ', 'Governorate').
	subdivision('eg-pts', 'eg', 'Būr Sa‘īd', 'Governorate').
	subdivision('eg-shg', 'eg', 'Sūhāj', 'Governorate').
	subdivision('eg-shr', 'eg', 'Ash Sharqīyah', 'Governorate').
	subdivision('eg-sin', 'eg', 'Shamāl Sīnā''', 'Governorate').
	subdivision('eg-suz', 'eg', 'As Suways', 'Governorate').
	subdivision('eg-wad', 'eg', 'Al Wādī al Jadīd', 'Governorate').
	subdivision('er-an', 'er', 'Ansabā', 'Region').
	subdivision('er-dk', 'er', 'Janūbī al Baḩrī al Aḩmar', 'Region').
	subdivision('er-du', 'er', 'Al Janūbī', 'Region').
	subdivision('er-gb', 'er', 'Qāsh-Barkah', 'Region').
	subdivision('er-ma', 'er', 'Al Awsaţ', 'Region').
	subdivision('er-sk', 'er', 'Shimālī al Baḩrī al Aḩmar', 'Region').
	subdivision('es-a', 'es', 'Alicante', 'Province').
	subdivision('es-ab', 'es', 'Albacete', 'Province').
	subdivision('es-al', 'es', 'Almería', 'Province').
	subdivision('es-an', 'es', 'Andalucía', 'Autonomous community').
	subdivision('es-ar', 'es', 'Aragón', 'Autonomous community').
	subdivision('es-as', 'es', 'Asturias, Principado de', 'Autonomous community').
	subdivision('es-av', 'es', 'Ávila', 'Province').
	subdivision('es-b', 'es', 'Barcelona [Barcelona]', 'Province').
	subdivision('es-ba', 'es', 'Badajoz', 'Province').
	subdivision('es-bi', 'es', 'Bizkaia', 'Province').
	subdivision('es-bu', 'es', 'Burgos', 'Province').
	subdivision('es-c', 'es', 'A Coruña [La Coruña]', 'Province').
	subdivision('es-ca', 'es', 'Cádiz', 'Province').
	subdivision('es-cb', 'es', 'Cantabria', 'Autonomous community').
	subdivision('es-cc', 'es', 'Cáceres', 'Province').
	subdivision('es-ce', 'es', 'Ceuta', 'Autonomous city in north africa').
	subdivision('es-cl', 'es', 'Castilla y León', 'Autonomous community').
	subdivision('es-cm', 'es', 'Castilla-La Mancha', 'Autonomous community').
	subdivision('es-cn', 'es', 'Canarias', 'Autonomous community').
	subdivision('es-co', 'es', 'Córdoba', 'Province').
	subdivision('es-cr', 'es', 'Ciudad Real', 'Province').
	subdivision('es-cs', 'es', 'Castellón', 'Province').
	subdivision('es-ct', 'es', 'Catalunya [Cataluña]', 'Autonomous community').
	subdivision('es-cu', 'es', 'Cuenca', 'Province').
	subdivision('es-ex', 'es', 'Extremadura', 'Autonomous community').
	subdivision('es-ga', 'es', 'Galicia [Galicia]', 'Autonomous community').
	subdivision('es-gc', 'es', 'Las Palmas', 'Province').
	subdivision('es-gi', 'es', 'Girona [Gerona]', 'Province').
	subdivision('es-gr', 'es', 'Granada', 'Province').
	subdivision('es-gu', 'es', 'Guadalajara', 'Province').
	subdivision('es-h', 'es', 'Huelva', 'Province').
	subdivision('es-hu', 'es', 'Huesca', 'Province').
	subdivision('es-ib', 'es', 'Illes Balears [Islas Baleares]', 'Autonomous community').
	subdivision('es-j', 'es', 'Jaén', 'Province').
	subdivision('es-l', 'es', 'Lleida [Lérida]', 'Province').
	subdivision('es-le', 'es', 'León', 'Province').
	subdivision('es-lo', 'es', 'La Rioja', 'Province').
	subdivision('es-lu', 'es', 'Lugo [Lugo]', 'Province').
	subdivision('es-m', 'es', 'Madrid', 'Province').
	subdivision('es-ma', 'es', 'Málaga', 'Province').
	subdivision('es-mc', 'es', 'Murcia, Región de', 'Autonomous community').
	subdivision('es-md', 'es', 'Madrid, Comunidad de', 'Autonomous community').
	subdivision('es-ml', 'es', 'Melilla', 'Autonomous city in north africa').
	subdivision('es-mu', 'es', 'Murcia', 'Province').
	subdivision('es-na', 'es', 'Navarra', 'Province').
	subdivision('es-nc', 'es', 'Navarra, Comunidad Foral de', 'Autonomous community').
	subdivision('es-o', 'es', 'Asturias', 'Province').
	subdivision('es-or', 'es', 'Ourense [Orense]', 'Province').
	subdivision('es-p', 'es', 'Palencia', 'Province').
	subdivision('es-pm', 'es', 'Illes Balears [Islas Baleares]', 'Province').
	subdivision('es-po', 'es', 'Pontevedra [Pontevedra]', 'Province').
	subdivision('es-pv', 'es', 'País Vasco', 'Autonomous community').
	subdivision('es-ri', 'es', 'La Rioja', 'Autonomous community').
	subdivision('es-s', 'es', 'Cantabria', 'Province').
	subdivision('es-sa', 'es', 'Salamanca', 'Province').
	subdivision('es-se', 'es', 'Sevilla', 'Province').
	subdivision('es-sg', 'es', 'Segovia', 'Province').
	subdivision('es-so', 'es', 'Soria', 'Province').
	subdivision('es-ss', 'es', 'Gipuzkoa', 'Province').
	subdivision('es-t', 'es', 'Tarragona [Tarragona]', 'Province').
	subdivision('es-te', 'es', 'Teruel', 'Province').
	subdivision('es-tf', 'es', 'Santa Cruz de Tenerife', 'Province').
	subdivision('es-to', 'es', 'Toledo', 'Province').
	subdivision('es-v', 'es', 'Valencia', 'Province').
	subdivision('es-va', 'es', 'Valladolid', 'Province').
	subdivision('es-vc', 'es', 'Valenciana, Comunidad', 'Autonomous community').
	subdivision('es-vi', 'es', 'Álava', 'Province').
	subdivision('es-z', 'es', 'Zaragoza', 'Province').
	subdivision('es-za', 'es', 'Zamora', 'Province').
	subdivision('et-aa', 'et', 'Addis Ababa', 'Administration').
	subdivision('et-af', 'et', 'Afar', 'Regional state').
	subdivision('et-am', 'et', 'Amara', 'Regional state').
	subdivision('et-be', 'et', 'Benshangul-Gumaz', 'Regional state').
	subdivision('et-dd', 'et', 'Dire Dawa', 'Administration').
	subdivision('et-ga', 'et', 'Gambela Peoples', 'Regional state').
	subdivision('et-ha', 'et', 'Harari People', 'Regional state').
	subdivision('et-or', 'et', 'Oromia', 'Regional state').
	subdivision('et-si', 'et', 'Sidama', 'Regional state').
	subdivision('et-sn', 'et', 'Southern Nations, Nationalities and Peoples', 'Regional state').
	subdivision('et-so', 'et', 'Somali', 'Regional state').
	subdivision('et-sw', 'et', 'Southwest Ethiopia Peoples', 'Regional state').
	subdivision('et-ti', 'et', 'Tigrai', 'Regional state').
	subdivision('fi-01', 'fi', 'Landskapet Åland', 'Region').
	subdivision('fi-02', 'fi', 'Etelä-Karjala', 'Region').
	subdivision('fi-03', 'fi', 'Etelä-Pohjanmaa', 'Region').
	subdivision('fi-04', 'fi', 'Etelä-Savo', 'Region').
	subdivision('fi-05', 'fi', 'Kainuu', 'Region').
	subdivision('fi-06', 'fi', 'Kanta-Häme', 'Region').
	subdivision('fi-07', 'fi', 'Keski-Pohjanmaa', 'Region').
	subdivision('fi-08', 'fi', 'Keski-Suomi', 'Region').
	subdivision('fi-09', 'fi', 'Kymenlaakso', 'Region').
	subdivision('fi-10', 'fi', 'Lappi', 'Region').
	subdivision('fi-11', 'fi', 'Pirkanmaa', 'Region').
	subdivision('fi-12', 'fi', 'Pohjanmaa', 'Region').
	subdivision('fi-13', 'fi', 'Pohjois-Karjala', 'Region').
	subdivision('fi-14', 'fi', 'Pohjois-Pohjanmaa', 'Region').
	subdivision('fi-15', 'fi', 'Pohjois-Savo', 'Region').
	subdivision('fi-16', 'fi', 'Päijät-Häme', 'Region').
	subdivision('fi-17', 'fi', 'Satakunta', 'Region').
	subdivision('fi-18', 'fi', 'Uusimaa', 'Region').
	subdivision('fi-19', 'fi', 'Varsinais-Suomi', 'Region').
	subdivision('fj-01', 'fj', 'Ba', 'Province').
	subdivision('fj-02', 'fj', 'Bua', 'Province').
	subdivision('fj-03', 'fj', 'Cakaudrove', 'Province').
	subdivision('fj-04', 'fj', 'Kadavu', 'Province').
	subdivision('fj-05', 'fj', 'Lau', 'Province').
	subdivision('fj-06', 'fj', 'Lomaiviti', 'Province').
	subdivision('fj-07', 'fj', 'Macuata', 'Province').
	subdivision('fj-08', 'fj', 'Nadroga and Navosa', 'Province').
	subdivision('fj-09', 'fj', 'Naitasiri', 'Province').
	subdivision('fj-10', 'fj', 'Namosi', 'Province').
	subdivision('fj-11', 'fj', 'Ra', 'Province').
	subdivision('fj-12', 'fj', 'Rewa', 'Province').
	subdivision('fj-13', 'fj', 'Serua', 'Province').
	subdivision('fj-14', 'fj', 'Tailevu', 'Province').
	subdivision('fj-c', 'fj', 'Central', 'Division').
	subdivision('fj-e', 'fj', 'Eastern', 'Division').
	subdivision('fj-n', 'fj', 'Northern', 'Division').
	subdivision('fj-r', 'fj', 'Rotuma', 'Dependency').
	subdivision('fj-w', 'fj', 'Western', 'Division').
	subdivision('fm-ksa', 'fm', 'Kosrae', 'State').
	subdivision('fm-pni', 'fm', 'Pohnpei', 'State').
	subdivision('fm-trk', 'fm', 'Chuuk', 'State').
	subdivision('fm-yap', 'fm', 'Yap', 'State').
	subdivision('fr-01', 'fr', 'Ain', 'Metropolitan department').
	subdivision('fr-02', 'fr', 'Aisne', 'Metropolitan department').
	subdivision('fr-03', 'fr', 'Allier', 'Metropolitan department').
	subdivision('fr-04', 'fr', 'Alpes-de-Haute-Provence', 'Metropolitan department').
	subdivision('fr-05', 'fr', 'Hautes-Alpes', 'Metropolitan department').
	subdivision('fr-06', 'fr', 'Alpes-Maritimes', 'Metropolitan department').
	subdivision('fr-07', 'fr', 'Ardèche', 'Metropolitan department').
	subdivision('fr-08', 'fr', 'Ardennes', 'Metropolitan department').
	subdivision('fr-09', 'fr', 'Ariège', 'Metropolitan department').
	subdivision('fr-10', 'fr', 'Aube', 'Metropolitan department').
	subdivision('fr-11', 'fr', 'Aude', 'Metropolitan department').
	subdivision('fr-12', 'fr', 'Aveyron', 'Metropolitan department').
	subdivision('fr-13', 'fr', 'Bouches-du-Rhône', 'Metropolitan department').
	subdivision('fr-14', 'fr', 'Calvados', 'Metropolitan department').
	subdivision('fr-15', 'fr', 'Cantal', 'Metropolitan department').
	subdivision('fr-16', 'fr', 'Charente', 'Metropolitan department').
	subdivision('fr-17', 'fr', 'Charente-Maritime', 'Metropolitan department').
	subdivision('fr-18', 'fr', 'Cher', 'Metropolitan department').
	subdivision('fr-19', 'fr', 'Corrèze', 'Metropolitan department').
	subdivision('fr-20r', 'fr', 'Corse', 'Metropolitan collectivity with special status').
	subdivision('fr-21', 'fr', 'Côte-d''Or', 'Metropolitan department').
	subdivision('fr-22', 'fr', 'Côtes-d''Armor', 'Metropolitan department').
	subdivision('fr-23', 'fr', 'Creuse', 'Metropolitan department').
	subdivision('fr-24', 'fr', 'Dordogne', 'Metropolitan department').
	subdivision('fr-25', 'fr', 'Doubs', 'Metropolitan department').
	subdivision('fr-26', 'fr', 'Drôme', 'Metropolitan department').
	subdivision('fr-27', 'fr', 'Eure', 'Metropolitan department').
	subdivision('fr-28', 'fr', 'Eure-et-Loir', 'Metropolitan department').
	subdivision('fr-29', 'fr', 'Finistère', 'Metropolitan department').
	subdivision('fr-2a', 'fr', 'Corse-du-Sud', 'Metropolitan department').
	subdivision('fr-2b', 'fr', 'Haute-Corse', 'Metropolitan department').
	subdivision('fr-30', 'fr', 'Gard', 'Metropolitan department').
	subdivision('fr-31', 'fr', 'Haute-Garonne', 'Metropolitan department').
	subdivision('fr-32', 'fr', 'Gers', 'Metropolitan department').
	subdivision('fr-33', 'fr', 'Gironde', 'Metropolitan department').
	subdivision('fr-34', 'fr', 'Hérault', 'Metropolitan department').
	subdivision('fr-35', 'fr', 'Ille-et-Vilaine', 'Metropolitan department').
	subdivision('fr-36', 'fr', 'Indre', 'Metropolitan department').
	subdivision('fr-37', 'fr', 'Indre-et-Loire', 'Metropolitan department').
	subdivision('fr-38', 'fr', 'Isère', 'Metropolitan department').
	subdivision('fr-39', 'fr', 'Jura', 'Metropolitan department').
	subdivision('fr-40', 'fr', 'Landes', 'Metropolitan department').
	subdivision('fr-41', 'fr', 'Loir-et-Cher', 'Metropolitan department').
	subdivision('fr-42', 'fr', 'Loire', 'Metropolitan department').
	subdivision('fr-43', 'fr', 'Haute-Loire', 'Metropolitan department').
	subdivision('fr-44', 'fr', 'Loire-Atlantique', 'Metropolitan department').
	subdivision('fr-45', 'fr', 'Loiret', 'Metropolitan department').
	subdivision('fr-46', 'fr', 'Lot', 'Metropolitan department').
	subdivision('fr-47', 'fr', 'Lot-et-Garonne', 'Metropolitan department').
	subdivision('fr-48', 'fr', 'Lozère', 'Metropolitan department').
	subdivision('fr-49', 'fr', 'Maine-et-Loire', 'Metropolitan department').
	subdivision('fr-50', 'fr', 'Manche', 'Metropolitan department').
	subdivision('fr-51', 'fr', 'Marne', 'Metropolitan department').
	subdivision('fr-52', 'fr', 'Haute-Marne', 'Metropolitan department').
	subdivision('fr-53', 'fr', 'Mayenne', 'Metropolitan department').
	subdivision('fr-54', 'fr', 'Meurthe-et-Moselle', 'Metropolitan department').
	subdivision('fr-55', 'fr', 'Meuse', 'Metropolitan department').
	subdivision('fr-56', 'fr', 'Morbihan', 'Metropolitan department').
	subdivision('fr-57', 'fr', 'Moselle', 'Metropolitan department').
	subdivision('fr-58', 'fr', 'Nièvre', 'Metropolitan department').
	subdivision('fr-59', 'fr', 'Nord', 'Metropolitan department').
	subdivision('fr-60', 'fr', 'Oise', 'Metropolitan department').
	subdivision('fr-61', 'fr', 'Orne', 'Metropolitan department').
	subdivision('fr-62', 'fr', 'Pas-de-Calais', 'Metropolitan department').
	subdivision('fr-63', 'fr', 'Puy-de-Dôme', 'Metropolitan department').
	subdivision('fr-64', 'fr', 'Pyrénées-Atlantiques', 'Metropolitan department').
	subdivision('fr-65', 'fr', 'Hautes-Pyrénées', 'Metropolitan department').
	subdivision('fr-66', 'fr', 'Pyrénées-Orientales', 'Metropolitan department').
	subdivision('fr-67', 'fr', 'Bas-Rhin', 'Metropolitan department').
	subdivision('fr-68', 'fr', 'Haut-Rhin', 'Metropolitan department').
	subdivision('fr-69', 'fr', 'Rhône', 'Metropolitan department').
	subdivision('fr-69m', 'fr', 'Métropole de Lyon', 'Metropolitan collectivity with special status').
	subdivision('fr-6ae', 'fr', 'Alsace', 'European collectivity').
	subdivision('fr-70', 'fr', 'Haute-Saône', 'Metropolitan department').
	subdivision('fr-71', 'fr', 'Saône-et-Loire', 'Metropolitan department').
	subdivision('fr-72', 'fr', 'Sarthe', 'Metropolitan department').
	subdivision('fr-73', 'fr', 'Savoie', 'Metropolitan department').
	subdivision('fr-74', 'fr', 'Haute-Savoie', 'Metropolitan department').
	subdivision('fr-75c', 'fr', 'Paris', 'Metropolitan collectivity with special status').
	subdivision('fr-76', 'fr', 'Seine-Maritime', 'Metropolitan department').
	subdivision('fr-77', 'fr', 'Seine-et-Marne', 'Metropolitan department').
	subdivision('fr-78', 'fr', 'Yvelines', 'Metropolitan department').
	subdivision('fr-79', 'fr', 'Deux-Sèvres', 'Metropolitan department').
	subdivision('fr-80', 'fr', 'Somme', 'Metropolitan department').
	subdivision('fr-81', 'fr', 'Tarn', 'Metropolitan department').
	subdivision('fr-82', 'fr', 'Tarn-et-Garonne', 'Metropolitan department').
	subdivision('fr-83', 'fr', 'Var', 'Metropolitan department').
	subdivision('fr-84', 'fr', 'Vaucluse', 'Metropolitan department').
	subdivision('fr-85', 'fr', 'Vendée', 'Metropolitan department').
	subdivision('fr-86', 'fr', 'Vienne', 'Metropolitan department').
	subdivision('fr-87', 'fr', 'Haute-Vienne', 'Metropolitan department').
	subdivision('fr-88', 'fr', 'Vosges', 'Metropolitan department').
	subdivision('fr-89', 'fr', 'Yonne', 'Metropolitan department').
	subdivision('fr-90', 'fr', 'Territoire de Belfort', 'Metropolitan department').
	subdivision('fr-91', 'fr', 'Essonne', 'Metropolitan department').
	subdivision('fr-92', 'fr', 'Hauts-de-Seine', 'Metropolitan department').
	subdivision('fr-93', 'fr', 'Seine-Saint-Denis', 'Metropolitan department').
	subdivision('fr-94', 'fr', 'Val-de-Marne', 'Metropolitan department').
	subdivision('fr-95', 'fr', 'Val-d''Oise', 'Metropolitan department').
	subdivision('fr-971', 'fr', 'Guadeloupe', 'Overseas departmental collectivity').
	subdivision('fr-972', 'fr', 'Martinique', 'Overseas unique territorial collectivity').
	subdivision('fr-973', 'fr', 'Guyane (française)', 'Overseas unique territorial collectivity').
	subdivision('fr-974', 'fr', 'La Réunion', 'Overseas departmental collectivity').
	subdivision('fr-976', 'fr', 'Mayotte', 'Overseas departmental collectivity').
	subdivision('fr-ara', 'fr', 'Auvergne-Rhône-Alpes', 'Metropolitan region').
	subdivision('fr-bfc', 'fr', 'Bourgogne-Franche-Comté', 'Metropolitan region').
	subdivision('fr-bl', 'fr', 'Saint-Barthélemy', 'Overseas collectivity').
	subdivision('fr-bre', 'fr', 'Bretagne', 'Metropolitan region').
	subdivision('fr-cp', 'fr', 'Clipperton', 'Dependency').
	subdivision('fr-cvl', 'fr', 'Centre-Val de Loire', 'Metropolitan region').
	subdivision('fr-ges', 'fr', 'Grand-Est', 'Metropolitan region').
	subdivision('fr-hdf', 'fr', 'Hauts-de-France', 'Metropolitan region').
	subdivision('fr-idf', 'fr', 'Île-de-France', 'Metropolitan region').
	subdivision('fr-mf', 'fr', 'Saint-Martin', 'Overseas collectivity').
	subdivision('fr-naq', 'fr', 'Nouvelle-Aquitaine', 'Metropolitan region').
	subdivision('fr-nc', 'fr', 'Nouvelle-Calédonie', 'Overseas collectivity with special status').
	subdivision('fr-nor', 'fr', 'Normandie', 'Metropolitan region').
	subdivision('fr-occ', 'fr', 'Occitanie', 'Metropolitan region').
	subdivision('fr-pac', 'fr', 'Provence-Alpes-Côte-d’Azur', 'Metropolitan region').
	subdivision('fr-pdl', 'fr', 'Pays-de-la-Loire', 'Metropolitan region').
	subdivision('fr-pf', 'fr', 'Polynésie française', 'Overseas collectivity').
	subdivision('fr-pm', 'fr', 'Saint-Pierre-et-Miquelon', 'Overseas collectivity').
	subdivision('fr-tf', 'fr', 'Terres australes françaises', 'Overseas territory').
	subdivision('fr-wf', 'fr', 'Wallis-et-Futuna', 'Overseas collectivity').
	subdivision('ga-1', 'ga', 'Estuaire', 'Province').
	subdivision('ga-2', 'ga', 'Haut-Ogooué', 'Province').
	subdivision('ga-3', 'ga', 'Moyen-Ogooué', 'Province').
	subdivision('ga-4', 'ga', 'Ngounié', 'Province').
	subdivision('ga-5', 'ga', 'Nyanga', 'Province').
	subdivision('ga-6', 'ga', 'Ogooué-Ivindo', 'Province').
	subdivision('ga-7', 'ga', 'Ogooué-Lolo', 'Province').
	subdivision('ga-8', 'ga', 'Ogooué-Maritime', 'Province').
	subdivision('ga-9', 'ga', 'Woleu-Ntem', 'Province').
	subdivision('gb-abc', 'gb', 'Armagh City, Banbridge and Craigavon', 'District').
	subdivision('gb-abd', 'gb', 'Aberdeenshire', 'Council area').
	subdivision('gb-abe', 'gb', 'Aberdeen City', 'Council area').
	subdivision('gb-agb', 'gb', 'Argyll and Bute', 'Council area').
	subdivision('gb-agy', 'gb', 'Isle of Anglesey [Sir Ynys Môn GB-YNM]', 'Unitary authority').
	subdivision('gb-and', 'gb', 'Ards and North Down', 'District').
	subdivision('gb-ann', 'gb', 'Antrim and Newtownabbey', 'District').
	subdivision('gb-ans', 'gb', 'Angus', 'Council area').
	subdivision('gb-bas', 'gb', 'Bath and North East Somerset', 'Unitary authority').
	subdivision('gb-bbd', 'gb', 'Blackburn with Darwen', 'Unitary authority').
	subdivision('gb-bcp', 'gb', 'Bournemouth, Christchurch and Poole', 'Unitary authority').
	subdivision('gb-bdf', 'gb', 'Bedford', 'Unitary authority').
	subdivision('gb-bdg', 'gb', 'Barking and Dagenham', 'London borough').
	subdivision('gb-ben', 'gb', 'Brent', 'London borough').
	subdivision('gb-bex', 'gb', 'Bexley', 'London borough').
	subdivision('gb-bfs', 'gb', 'Belfast City', 'District').
	subdivision('gb-bge', 'gb', 'Bridgend [Pen-y-bont ar Ogwr GB-POG]', 'Unitary authority').
	subdivision('gb-bgw', 'gb', 'Blaenau Gwent', 'Unitary authority').
	subdivision('gb-bir', 'gb', 'Birmingham', 'Metropolitan district').
	subdivision('gb-bkm', 'gb', 'Buckinghamshire', 'Unitary authority').
	subdivision('gb-bne', 'gb', 'Barnet', 'London borough').
	subdivision('gb-bnh', 'gb', 'Brighton and Hove', 'Unitary authority').
	subdivision('gb-bns', 'gb', 'Barnsley', 'Metropolitan district').
	subdivision('gb-bol', 'gb', 'Bolton', 'Metropolitan district').
	subdivision('gb-bpl', 'gb', 'Blackpool', 'Unitary authority').
	subdivision('gb-brc', 'gb', 'Bracknell Forest', 'Unitary authority').
	subdivision('gb-brd', 'gb', 'Bradford', 'Metropolitan district').
	subdivision('gb-bry', 'gb', 'Bromley', 'London borough').
	subdivision('gb-bst', 'gb', 'Bristol, City of', 'Unitary authority').
	subdivision('gb-bur', 'gb', 'Bury', 'Metropolitan district').
	subdivision('gb-cam', 'gb', 'Cambridgeshire', 'Two-tier county').
	subdivision('gb-cay', 'gb', 'Caerphilly [Caerffili GB-CAF]', 'Unitary authority').
	subdivision('gb-cbf', 'gb', 'Central Bedfordshire', 'Unitary authority').
	subdivision('gb-ccg', 'gb', 'Causeway Coast and Glens', 'District').
	subdivision('gb-cgn', 'gb', 'Ceredigion [Sir Ceredigion]', 'Unitary authority').
	subdivision('gb-che', 'gb', 'Cheshire East', 'Unitary authority').
	subdivision('gb-chw', 'gb', 'Cheshire West and Chester', 'Unitary authority').
	subdivision('gb-cld', 'gb', 'Calderdale', 'Metropolitan district').
	subdivision('gb-clk', 'gb', 'Clackmannanshire', 'Council area').
	subdivision('gb-cma', 'gb', 'Cumbria', 'Two-tier county').
	subdivision('gb-cmd', 'gb', 'Camden', 'London borough').
	subdivision('gb-cmn', 'gb', 'Carmarthenshire [Sir Gaerfyrddin GB-GFY]', 'Unitary authority').
	subdivision('gb-con', 'gb', 'Cornwall', 'Unitary authority').
	subdivision('gb-cov', 'gb', 'Coventry', 'Metropolitan district').
	subdivision('gb-crf', 'gb', 'Cardiff [Caerdydd GB-CRD]', 'Unitary authority').
	subdivision('gb-cry', 'gb', 'Croydon', 'London borough').
	subdivision('gb-cwy', 'gb', 'Conwy', 'Unitary authority').
	subdivision('gb-dal', 'gb', 'Darlington', 'Unitary authority').
	subdivision('gb-dby', 'gb', 'Derbyshire', 'Two-tier county').
	subdivision('gb-den', 'gb', 'Denbighshire [Sir Ddinbych GB-DDB]', 'Unitary authority').
	subdivision('gb-der', 'gb', 'Derby', 'Unitary authority').
	subdivision('gb-dev', 'gb', 'Devon', 'Two-tier county').
	subdivision('gb-dgy', 'gb', 'Dumfries and Galloway', 'Council area').
	subdivision('gb-dnc', 'gb', 'Doncaster', 'Metropolitan district').
	subdivision('gb-dnd', 'gb', 'Dundee City', 'Council area').
	subdivision('gb-dor', 'gb', 'Dorset', 'Two-tier county').
	subdivision('gb-drs', 'gb', 'Derry and Strabane', 'District').
	subdivision('gb-dud', 'gb', 'Dudley', 'Metropolitan district').
	subdivision('gb-dur', 'gb', 'Durham, County', 'Unitary authority').
	subdivision('gb-eal', 'gb', 'Ealing', 'London borough').
	subdivision('gb-eay', 'gb', 'East Ayrshire', 'Council area').
	subdivision('gb-edh', 'gb', 'Edinburgh, City of', 'Council area').
	subdivision('gb-edu', 'gb', 'East Dunbartonshire', 'Council area').
	subdivision('gb-eln', 'gb', 'East Lothian', 'Council area').
	subdivision('gb-els', 'gb', 'Eilean Siar', 'Council area').
	subdivision('gb-enf', 'gb', 'Enfield', 'London borough').
	subdivision('gb-eng', 'gb', 'England', 'Country').
	subdivision('gb-erw', 'gb', 'East Renfrewshire', 'Council area').
	subdivision('gb-ery', 'gb', 'East Riding of Yorkshire', 'Unitary authority').
	subdivision('gb-ess', 'gb', 'Essex', 'Two-tier county').
	subdivision('gb-esx', 'gb', 'East Sussex', 'Two-tier county').
	subdivision('gb-fal', 'gb', 'Falkirk', 'Council area').
	subdivision('gb-fif', 'gb', 'Fife', 'Council area').
	subdivision('gb-fln', 'gb', 'Flintshire [Sir y Fflint GB-FFL]', 'Unitary authority').
	subdivision('gb-fmo', 'gb', 'Fermanagh and Omagh', 'District').
	subdivision('gb-gat', 'gb', 'Gateshead', 'Metropolitan district').
	subdivision('gb-glg', 'gb', 'Glasgow City', 'Council area').
	subdivision('gb-gls', 'gb', 'Gloucestershire', 'Two-tier county').
	subdivision('gb-gre', 'gb', 'Greenwich', 'London borough').
	subdivision('gb-gwn', 'gb', 'Gwynedd', 'Unitary authority').
	subdivision('gb-hal', 'gb', 'Halton', 'Unitary authority').
	subdivision('gb-ham', 'gb', 'Hampshire', 'Two-tier county').
	subdivision('gb-hav', 'gb', 'Havering', 'London borough').
	subdivision('gb-hck', 'gb', 'Hackney', 'London borough').
	subdivision('gb-hef', 'gb', 'Herefordshire', 'Unitary authority').
	subdivision('gb-hil', 'gb', 'Hillingdon', 'London borough').
	subdivision('gb-hld', 'gb', 'Highland', 'Council area').
	subdivision('gb-hmf', 'gb', 'Hammersmith and Fulham', 'London borough').
	subdivision('gb-hns', 'gb', 'Hounslow', 'London borough').
	subdivision('gb-hpl', 'gb', 'Hartlepool', 'Unitary authority').
	subdivision('gb-hrt', 'gb', 'Hertfordshire', 'Two-tier county').
	subdivision('gb-hrw', 'gb', 'Harrow', 'London borough').
	subdivision('gb-hry', 'gb', 'Haringey', 'London borough').
	subdivision('gb-ios', 'gb', 'Isles of Scilly', 'Unitary authority').
	subdivision('gb-iow', 'gb', 'Isle of Wight', 'Unitary authority').
	subdivision('gb-isl', 'gb', 'Islington', 'London borough').
	subdivision('gb-ivc', 'gb', 'Inverclyde', 'Council area').
	subdivision('gb-kec', 'gb', 'Kensington and Chelsea', 'London borough').
	subdivision('gb-ken', 'gb', 'Kent', 'Two-tier county').
	subdivision('gb-khl', 'gb', 'Kingston upon Hull', 'Unitary authority').
	subdivision('gb-kir', 'gb', 'Kirklees', 'Metropolitan district').
	subdivision('gb-ktt', 'gb', 'Kingston upon Thames', 'London borough').
	subdivision('gb-kwl', 'gb', 'Knowsley', 'Metropolitan district').
	subdivision('gb-lan', 'gb', 'Lancashire', 'Two-tier county').
	subdivision('gb-lbc', 'gb', 'Lisburn and Castlereagh', 'District').
	subdivision('gb-lbh', 'gb', 'Lambeth', 'London borough').
	subdivision('gb-lce', 'gb', 'Leicester', 'Unitary authority').
	subdivision('gb-lds', 'gb', 'Leeds', 'Metropolitan district').
	subdivision('gb-lec', 'gb', 'Leicestershire', 'Two-tier county').
	subdivision('gb-lew', 'gb', 'Lewisham', 'London borough').
	subdivision('gb-lin', 'gb', 'Lincolnshire', 'Two-tier county').
	subdivision('gb-liv', 'gb', 'Liverpool', 'Metropolitan district').
	subdivision('gb-lnd', 'gb', 'London, City of', 'City corporation').
	subdivision('gb-lut', 'gb', 'Luton', 'Unitary authority').
	subdivision('gb-man', 'gb', 'Manchester', 'Metropolitan district').
	subdivision('gb-mdb', 'gb', 'Middlesbrough', 'Unitary authority').
	subdivision('gb-mdw', 'gb', 'Medway', 'Unitary authority').
	subdivision('gb-mea', 'gb', 'Mid and East Antrim', 'District').
	subdivision('gb-mik', 'gb', 'Milton Keynes', 'Unitary authority').
	subdivision('gb-mln', 'gb', 'Midlothian', 'Council area').
	subdivision('gb-mon', 'gb', 'Monmouthshire [Sir Fynwy GB-FYN]', 'Unitary authority').
	subdivision('gb-mrt', 'gb', 'Merton', 'London borough').
	subdivision('gb-mry', 'gb', 'Moray', 'Council area').
	subdivision('gb-mty', 'gb', 'Merthyr Tydfil [Merthyr Tudful GB-MTU]', 'Unitary authority').
	subdivision('gb-mul', 'gb', 'Mid-Ulster', 'District').
	subdivision('gb-nay', 'gb', 'North Ayrshire', 'Council area').
	subdivision('gb-nbl', 'gb', 'Northumberland', 'Unitary authority').
	subdivision('gb-nel', 'gb', 'North East Lincolnshire', 'Unitary authority').
	subdivision('gb-net', 'gb', 'Newcastle upon Tyne', 'Metropolitan district').
	subdivision('gb-nfk', 'gb', 'Norfolk', 'Two-tier county').
	subdivision('gb-ngm', 'gb', 'Nottingham', 'Unitary authority').
	subdivision('gb-nir', 'gb', 'Northern Ireland', 'Province').
	subdivision('gb-nlk', 'gb', 'North Lanarkshire', 'Council area').
	subdivision('gb-nln', 'gb', 'North Lincolnshire', 'Unitary authority').
	subdivision('gb-nmd', 'gb', 'Newry, Mourne and Down', 'District').
	subdivision('gb-nnh', 'gb', 'North Northamptonshire', 'Unitary authority').
	subdivision('gb-nsm', 'gb', 'North Somerset', 'Unitary authority').
	subdivision('gb-ntl', 'gb', 'Neath Port Talbot [Castell-nedd Port Talbot GB-CTL]', 'Unitary authority').
	subdivision('gb-ntt', 'gb', 'Nottinghamshire', 'Two-tier county').
	subdivision('gb-nty', 'gb', 'North Tyneside', 'Metropolitan district').
	subdivision('gb-nwm', 'gb', 'Newham', 'London borough').
	subdivision('gb-nwp', 'gb', 'Newport [Casnewydd GB-CNW]', 'Unitary authority').
	subdivision('gb-nyk', 'gb', 'North Yorkshire', 'Two-tier county').
	subdivision('gb-old', 'gb', 'Oldham', 'Metropolitan district').
	subdivision('gb-ork', 'gb', 'Orkney Islands', 'Council area').
	subdivision('gb-oxf', 'gb', 'Oxfordshire', 'Two-tier county').
	subdivision('gb-pem', 'gb', 'Pembrokeshire [Sir Benfro GB-BNF]', 'Unitary authority').
	subdivision('gb-pkn', 'gb', 'Perth and Kinross', 'Council area').
	subdivision('gb-ply', 'gb', 'Plymouth', 'Unitary authority').
	subdivision('gb-por', 'gb', 'Portsmouth', 'Unitary authority').
	subdivision('gb-pow', 'gb', 'Powys', 'Unitary authority').
	subdivision('gb-pte', 'gb', 'Peterborough', 'Unitary authority').
	subdivision('gb-rcc', 'gb', 'Redcar and Cleveland', 'Unitary authority').
	subdivision('gb-rch', 'gb', 'Rochdale', 'Metropolitan district').
	subdivision('gb-rct', 'gb', 'Rhondda Cynon Taff [Rhondda CynonTaf]', 'Unitary authority').
	subdivision('gb-rdb', 'gb', 'Redbridge', 'London borough').
	subdivision('gb-rdg', 'gb', 'Reading', 'Unitary authority').
	subdivision('gb-rfw', 'gb', 'Renfrewshire', 'Council area').
	subdivision('gb-ric', 'gb', 'Richmond upon Thames', 'London borough').
	subdivision('gb-rot', 'gb', 'Rotherham', 'Metropolitan district').
	subdivision('gb-rut', 'gb', 'Rutland', 'Unitary authority').
	subdivision('gb-saw', 'gb', 'Sandwell', 'Metropolitan district').
	subdivision('gb-say', 'gb', 'South Ayrshire', 'Council area').
	subdivision('gb-scb', 'gb', 'Scottish Borders', 'Council area').
	subdivision('gb-sct', 'gb', 'Scotland', 'Country').
	subdivision('gb-sfk', 'gb', 'Suffolk', 'Two-tier county').
	subdivision('gb-sft', 'gb', 'Sefton', 'Metropolitan district').
	subdivision('gb-sgc', 'gb', 'South Gloucestershire', 'Unitary authority').
	subdivision('gb-shf', 'gb', 'Sheffield', 'Metropolitan district').
	subdivision('gb-shn', 'gb', 'St. Helens', 'Metropolitan district').
	subdivision('gb-shr', 'gb', 'Shropshire', 'Unitary authority').
	subdivision('gb-skp', 'gb', 'Stockport', 'Metropolitan district').
	subdivision('gb-slf', 'gb', 'Salford', 'Metropolitan district').
	subdivision('gb-slg', 'gb', 'Slough', 'Unitary authority').
	subdivision('gb-slk', 'gb', 'South Lanarkshire', 'Council area').
	subdivision('gb-snd', 'gb', 'Sunderland', 'Metropolitan district').
	subdivision('gb-sol', 'gb', 'Solihull', 'Metropolitan district').
	subdivision('gb-som', 'gb', 'Somerset', 'Two-tier county').
	subdivision('gb-sos', 'gb', 'Southend-on-Sea', 'Unitary authority').
	subdivision('gb-sry', 'gb', 'Surrey', 'Two-tier county').
	subdivision('gb-ste', 'gb', 'Stoke-on-Trent', 'Unitary authority').
	subdivision('gb-stg', 'gb', 'Stirling', 'Council area').
	subdivision('gb-sth', 'gb', 'Southampton', 'Unitary authority').
	subdivision('gb-stn', 'gb', 'Sutton', 'London borough').
	subdivision('gb-sts', 'gb', 'Staffordshire', 'Two-tier county').
	subdivision('gb-stt', 'gb', 'Stockton-on-Tees', 'Unitary authority').
	subdivision('gb-sty', 'gb', 'South Tyneside', 'Metropolitan district').
	subdivision('gb-swa', 'gb', 'Swansea [Abertawe GB-ATA]', 'Unitary authority').
	subdivision('gb-swd', 'gb', 'Swindon', 'Unitary authority').
	subdivision('gb-swk', 'gb', 'Southwark', 'London borough').
	subdivision('gb-tam', 'gb', 'Tameside', 'Metropolitan district').
	subdivision('gb-tfw', 'gb', 'Telford and Wrekin', 'Unitary authority').
	subdivision('gb-thr', 'gb', 'Thurrock', 'Unitary authority').
	subdivision('gb-tob', 'gb', 'Torbay', 'Unitary authority').
	subdivision('gb-tof', 'gb', 'Torfaen [Tor-faen]', 'Unitary authority').
	subdivision('gb-trf', 'gb', 'Trafford', 'Metropolitan district').
	subdivision('gb-twh', 'gb', 'Tower Hamlets', 'London borough').
	subdivision('gb-vgl', 'gb', 'Vale of Glamorgan, The [Bro Morgannwg GB-BMG]', 'Unitary authority').
	subdivision('gb-war', 'gb', 'Warwickshire', 'Two-tier county').
	subdivision('gb-wbk', 'gb', 'West Berkshire', 'Unitary authority').
	subdivision('gb-wdu', 'gb', 'West Dunbartonshire', 'Council area').
	subdivision('gb-wft', 'gb', 'Waltham Forest', 'London borough').
	subdivision('gb-wgn', 'gb', 'Wigan', 'Metropolitan district').
	subdivision('gb-wil', 'gb', 'Wiltshire', 'Unitary authority').
	subdivision('gb-wkf', 'gb', 'Wakefield', 'Metropolitan district').
	subdivision('gb-wll', 'gb', 'Walsall', 'Metropolitan district').
	subdivision('gb-wln', 'gb', 'West Lothian', 'Council area').
	subdivision('gb-wls', 'gb', 'Wales [Cymru GB-CYM]', 'Country').
	subdivision('gb-wlv', 'gb', 'Wolverhampton', 'Metropolitan district').
	subdivision('gb-wnd', 'gb', 'Wandsworth', 'London borough').
	subdivision('gb-wnh', 'gb', 'West Northamptonshire', 'Unitary authority').
	subdivision('gb-wnm', 'gb', 'Windsor and Maidenhead', 'Unitary authority').
	subdivision('gb-wok', 'gb', 'Wokingham', 'Unitary authority').
	subdivision('gb-wor', 'gb', 'Worcestershire', 'Two-tier county').
	subdivision('gb-wrl', 'gb', 'Wirral', 'Metropolitan district').
	subdivision('gb-wrt', 'gb', 'Warrington', 'Unitary authority').
	subdivision('gb-wrx', 'gb', 'Wrexham [Wrecsam GB-WRC]', 'Unitary authority').
	subdivision('gb-wsm', 'gb', 'Westminster', 'London borough').
	subdivision('gb-wsx', 'gb', 'West Sussex', 'Two-tier county').
	subdivision('gb-yor', 'gb', 'York', 'Unitary authority').
	subdivision('gb-zet', 'gb', 'Shetland Islands', 'Council area').
	subdivision('gd-01', 'gd', 'Saint Andrew', 'Parish').
	subdivision('gd-02', 'gd', 'Saint David', 'Parish').
	subdivision('gd-03', 'gd', 'Saint George', 'Parish').
	subdivision('gd-04', 'gd', 'Saint John', 'Parish').
	subdivision('gd-05', 'gd', 'Saint Mark', 'Parish').
	subdivision('gd-06', 'gd', 'Saint Patrick', 'Parish').
	subdivision('gd-10', 'gd', 'Southern Grenadine Islands', 'Dependency').
	subdivision('ge-ab', 'ge', 'Abkhazia', 'Autonomous republic').
	subdivision('ge-aj', 'ge', 'Ajaria', 'Autonomous republic').
	subdivision('ge-gu', 'ge', 'Guria', 'Region').
	subdivision('ge-im', 'ge', 'Imereti', 'Region').
	subdivision('ge-ka', 'ge', 'K''akheti', 'Region').
	subdivision('ge-kk', 'ge', 'Kvemo Kartli', 'Region').
	subdivision('ge-mm', 'ge', 'Mtskheta-Mtianeti', 'Region').
	subdivision('ge-rl', 'ge', 'Rach''a-Lechkhumi-Kvemo Svaneti', 'Region').
	subdivision('ge-sj', 'ge', 'Samtskhe-Javakheti', 'Region').
	subdivision('ge-sk', 'ge', 'Shida Kartli', 'Region').
	subdivision('ge-sz', 'ge', 'Samegrelo-Zemo Svaneti', 'Region').
	subdivision('ge-tb', 'ge', 'Tbilisi', 'City').
	subdivision('gh-aa', 'gh', 'Greater Accra', 'Region').
	subdivision('gh-af', 'gh', 'Ahafo', 'Region').
	subdivision('gh-ah', 'gh', 'Ashanti', 'Region').
	subdivision('gh-be', 'gh', 'Bono East', 'Region').
	subdivision('gh-bo', 'gh', 'Bono', 'Region').
	subdivision('gh-cp', 'gh', 'Central', 'Region').
	subdivision('gh-ep', 'gh', 'Eastern', 'Region').
	subdivision('gh-ne', 'gh', 'North East', 'Region').
	subdivision('gh-np', 'gh', 'Northern', 'Region').
	subdivision('gh-ot', 'gh', 'Oti', 'Region').
	subdivision('gh-sv', 'gh', 'Savannah', 'Region').
	subdivision('gh-tv', 'gh', 'Volta', 'Region').
	subdivision('gh-ue', 'gh', 'Upper East', 'Region').
	subdivision('gh-uw', 'gh', 'Upper West', 'Region').
	subdivision('gh-wn', 'gh', 'Western North', 'Region').
	subdivision('gh-wp', 'gh', 'Western', 'Region').
	subdivision('gl-av', 'gl', 'Avannaata Kommunia', 'Municipality').
	subdivision('gl-ku', 'gl', 'Kommune Kujalleq', 'Municipality').
	subdivision('gl-qe', 'gl', 'Qeqqata Kommunia', 'Municipality').
	subdivision('gl-qt', 'gl', 'Kommune Qeqertalik', 'Municipality').
	subdivision('gl-sm', 'gl', 'Kommuneqarfik Sermersooq', 'Municipality').
	subdivision('gm-b', 'gm', 'Banjul', 'City').
	subdivision('gm-l', 'gm', 'Lower River', 'Division').
	subdivision('gm-m', 'gm', 'Central River', 'Division').
	subdivision('gm-n', 'gm', 'North Bank', 'Division').
	subdivision('gm-u', 'gm', 'Upper River', 'Division').
	subdivision('gm-w', 'gm', 'Western', 'Division').
	subdivision('gn-b', 'gn', 'Boké', 'Administrative region').
	subdivision('gn-be', 'gn', 'Beyla', 'Prefecture').
	subdivision('gn-bf', 'gn', 'Boffa', 'Prefecture').
	subdivision('gn-bk', 'gn', 'Boké', 'Prefecture').
	subdivision('gn-c', 'gn', 'Conakry', 'Governorate').
	subdivision('gn-co', 'gn', 'Coyah', 'Prefecture').
	subdivision('gn-d', 'gn', 'Kindia', 'Administrative region').
	subdivision('gn-db', 'gn', 'Dabola', 'Prefecture').
	subdivision('gn-di', 'gn', 'Dinguiraye', 'Prefecture').
	subdivision('gn-dl', 'gn', 'Dalaba', 'Prefecture').
	subdivision('gn-du', 'gn', 'Dubréka', 'Prefecture').
	subdivision('gn-f', 'gn', 'Faranah', 'Administrative region').
	subdivision('gn-fa', 'gn', 'Faranah', 'Prefecture').
	subdivision('gn-fo', 'gn', 'Forécariah', 'Prefecture').
	subdivision('gn-fr', 'gn', 'Fria', 'Prefecture').
	subdivision('gn-ga', 'gn', 'Gaoual', 'Prefecture').
	subdivision('gn-gu', 'gn', 'Guékédou', 'Prefecture').
	subdivision('gn-k', 'gn', 'Kankan', 'Administrative region').
	subdivision('gn-ka', 'gn', 'Kankan', 'Prefecture').
	subdivision('gn-kb', 'gn', 'Koubia', 'Prefecture').
	subdivision('gn-kd', 'gn', 'Kindia', 'Prefecture').
	subdivision('gn-ke', 'gn', 'Kérouané', 'Prefecture').
	subdivision('gn-kn', 'gn', 'Koundara', 'Prefecture').
	subdivision('gn-ko', 'gn', 'Kouroussa', 'Prefecture').
	subdivision('gn-ks', 'gn', 'Kissidougou', 'Prefecture').
	subdivision('gn-l', 'gn', 'Labé', 'Administrative region').
	subdivision('gn-la', 'gn', 'Labé', 'Prefecture').
	subdivision('gn-le', 'gn', 'Lélouma', 'Prefecture').
	subdivision('gn-lo', 'gn', 'Lola', 'Prefecture').
	subdivision('gn-m', 'gn', 'Mamou', 'Administrative region').
	subdivision('gn-mc', 'gn', 'Macenta', 'Prefecture').
	subdivision('gn-md', 'gn', 'Mandiana', 'Prefecture').
	subdivision('gn-ml', 'gn', 'Mali', 'Prefecture').
	subdivision('gn-mm', 'gn', 'Mamou', 'Prefecture').
	subdivision('gn-n', 'gn', 'Nzérékoré', 'Administrative region').
	subdivision('gn-nz', 'gn', 'Nzérékoré', 'Prefecture').
	subdivision('gn-pi', 'gn', 'Pita', 'Prefecture').
	subdivision('gn-si', 'gn', 'Siguiri', 'Prefecture').
	subdivision('gn-te', 'gn', 'Télimélé', 'Prefecture').
	subdivision('gn-to', 'gn', 'Tougué', 'Prefecture').
	subdivision('gn-yo', 'gn', 'Yomou', 'Prefecture').
	subdivision('gq-an', 'gq', 'Annobon', 'Province').
	subdivision('gq-bn', 'gq', 'Bioko Nord', 'Province').
	subdivision('gq-bs', 'gq', 'Bioko Sud', 'Province').
	subdivision('gq-c', 'gq', 'Région Continentale', 'Region').
	subdivision('gq-cs', 'gq', 'Centro Sud', 'Province').
	subdivision('gq-dj', 'gq', 'Djibloho', 'Province').
	subdivision('gq-i', 'gq', 'Région Insulaire', 'Region').
	subdivision('gq-kn', 'gq', 'Kié-Ntem', 'Province').
	subdivision('gq-li', 'gq', 'Littoral', 'Province').
	subdivision('gq-wn', 'gq', 'Wele-Nzas', 'Province').
	subdivision('gr-69', 'gr', 'Ágion Óros', 'Self-governed part').
	subdivision('gr-a', 'gr', 'Anatolikí Makedonía kai Thráki', 'Administrative region').
	subdivision('gr-b', 'gr', 'Kentrikí Makedonía', 'Administrative region').
	subdivision('gr-c', 'gr', 'Dytikí Makedonía', 'Administrative region').
	subdivision('gr-d', 'gr', 'Ípeiros', 'Administrative region').
	subdivision('gr-e', 'gr', 'Thessalía', 'Administrative region').
	subdivision('gr-f', 'gr', 'Ionía Nísia', 'Administrative region').
	subdivision('gr-g', 'gr', 'Dytikí Elláda', 'Administrative region').
	subdivision('gr-h', 'gr', 'Stereá Elláda', 'Administrative region').
	subdivision('gr-i', 'gr', 'Attikí', 'Administrative region').
	subdivision('gr-j', 'gr', 'Pelopónnisos', 'Administrative region').
	subdivision('gr-k', 'gr', 'Vóreio Aigaío', 'Administrative region').
	subdivision('gr-l', 'gr', 'Nótio Aigaío', 'Administrative region').
	subdivision('gr-m', 'gr', 'Kríti', 'Administrative region').
	subdivision('gt-01', 'gt', 'Guatemala', 'Department').
	subdivision('gt-02', 'gt', 'El Progreso', 'Department').
	subdivision('gt-03', 'gt', 'Sacatepéquez', 'Department').
	subdivision('gt-04', 'gt', 'Chimaltenango', 'Department').
	subdivision('gt-05', 'gt', 'Escuintla', 'Department').
	subdivision('gt-06', 'gt', 'Santa Rosa', 'Department').
	subdivision('gt-07', 'gt', 'Sololá', 'Department').
	subdivision('gt-08', 'gt', 'Totonicapán', 'Department').
	subdivision('gt-09', 'gt', 'Quetzaltenango', 'Department').
	subdivision('gt-10', 'gt', 'Suchitepéquez', 'Department').
	subdivision('gt-11', 'gt', 'Retalhuleu', 'Department').
	subdivision('gt-12', 'gt', 'San Marcos', 'Department').
	subdivision('gt-13', 'gt', 'Huehuetenango', 'Department').
	subdivision('gt-14', 'gt', 'Quiché', 'Department').
	subdivision('gt-15', 'gt', 'Baja Verapaz', 'Department').
	subdivision('gt-16', 'gt', 'Alta Verapaz', 'Department').
	subdivision('gt-17', 'gt', 'Petén', 'Department').
	subdivision('gt-18', 'gt', 'Izabal', 'Department').
	subdivision('gt-19', 'gt', 'Zacapa', 'Department').
	subdivision('gt-20', 'gt', 'Chiquimula', 'Department').
	subdivision('gt-21', 'gt', 'Jalapa', 'Department').
	subdivision('gt-22', 'gt', 'Jutiapa', 'Department').
	subdivision('gw-ba', 'gw', 'Bafatá', 'Region').
	subdivision('gw-bl', 'gw', 'Bolama / Bijagós', 'Region').
	subdivision('gw-bm', 'gw', 'Biombo', 'Region').
	subdivision('gw-bs', 'gw', 'Bissau', 'Autonomous sector').
	subdivision('gw-ca', 'gw', 'Cacheu', 'Region').
	subdivision('gw-ga', 'gw', 'Gabú', 'Region').
	subdivision('gw-l', 'gw', 'Leste', 'Province').
	subdivision('gw-n', 'gw', 'Norte', 'Province').
	subdivision('gw-oi', 'gw', 'Oio', 'Region').
	subdivision('gw-qu', 'gw', 'Quinara', 'Region').
	subdivision('gw-s', 'gw', 'Sul', 'Province').
	subdivision('gw-to', 'gw', 'Tombali', 'Region').
	subdivision('gy-ba', 'gy', 'Barima-Waini', 'Region').
	subdivision('gy-cu', 'gy', 'Cuyuni-Mazaruni', 'Region').
	subdivision('gy-de', 'gy', 'Demerara-Mahaica', 'Region').
	subdivision('gy-eb', 'gy', 'East Berbice-Corentyne', 'Region').
	subdivision('gy-es', 'gy', 'Essequibo Islands-West Demerara', 'Region').
	subdivision('gy-ma', 'gy', 'Mahaica-Berbice', 'Region').
	subdivision('gy-pm', 'gy', 'Pomeroon-Supenaam', 'Region').
	subdivision('gy-pt', 'gy', 'Potaro-Siparuni', 'Region').
	subdivision('gy-ud', 'gy', 'Upper Demerara-Berbice', 'Region').
	subdivision('gy-ut', 'gy', 'Upper Takutu-Upper Essequibo', 'Region').
	subdivision('hn-at', 'hn', 'Atlántida', 'Department').
	subdivision('hn-ch', 'hn', 'Choluteca', 'Department').
	subdivision('hn-cl', 'hn', 'Colón', 'Department').
	subdivision('hn-cm', 'hn', 'Comayagua', 'Department').
	subdivision('hn-cp', 'hn', 'Copán', 'Department').
	subdivision('hn-cr', 'hn', 'Cortés', 'Department').
	subdivision('hn-ep', 'hn', 'El Paraíso', 'Department').
	subdivision('hn-fm', 'hn', 'Francisco Morazán', 'Department').
	subdivision('hn-gd', 'hn', 'Gracias a Dios', 'Department').
	subdivision('hn-ib', 'hn', 'Islas de la Bahía', 'Department').
	subdivision('hn-in', 'hn', 'Intibucá', 'Department').
	subdivision('hn-le', 'hn', 'Lempira', 'Department').
	subdivision('hn-lp', 'hn', 'La Paz', 'Department').
	subdivision('hn-oc', 'hn', 'Ocotepeque', 'Department').
	subdivision('hn-ol', 'hn', 'Olancho', 'Department').
	subdivision('hn-sb', 'hn', 'Santa Bárbara', 'Department').
	subdivision('hn-va', 'hn', 'Valle', 'Department').
	subdivision('hn-yo', 'hn', 'Yoro', 'Department').
	subdivision('hr-01', 'hr', 'Zagrebačka županija', 'County').
	subdivision('hr-02', 'hr', 'Krapinsko-zagorska županija', 'County').
	subdivision('hr-03', 'hr', 'Sisačko-moslavačka županija', 'County').
	subdivision('hr-04', 'hr', 'Karlovačka županija', 'County').
	subdivision('hr-05', 'hr', 'Varaždinska županija', 'County').
	subdivision('hr-06', 'hr', 'Koprivničko-križevačka županija', 'County').
	subdivision('hr-07', 'hr', 'Bjelovarsko-bilogorska županija', 'County').
	subdivision('hr-08', 'hr', 'Primorsko-goranska županija', 'County').
	subdivision('hr-09', 'hr', 'Ličko-senjska županija', 'County').
	subdivision('hr-10', 'hr', 'Virovitičko-podravska županija', 'County').
	subdivision('hr-11', 'hr', 'Požeško-slavonska županija', 'County').
	subdivision('hr-12', 'hr', 'Brodsko-posavska županija', 'County').
	subdivision('hr-13', 'hr', 'Zadarska županija', 'County').
	subdivision('hr-14', 'hr', 'Osječko-baranjska županija', 'County').
	subdivision('hr-15', 'hr', 'Šibensko-kninska županija', 'County').
	subdivision('hr-16', 'hr', 'Vukovarsko-srijemska županija', 'County').
	subdivision('hr-17', 'hr', 'Splitsko-dalmatinska županija', 'County').
	subdivision('hr-18', 'hr', 'Istarska županija', 'County').
	subdivision('hr-19', 'hr', 'Dubrovačko-neretvanska županija', 'County').
	subdivision('hr-20', 'hr', 'Međimurska županija', 'County').
	subdivision('hr-21', 'hr', 'Grad Zagreb', 'City').
	subdivision('ht-ar', 'ht', 'Artibonite', 'Department').
	subdivision('ht-ce', 'ht', 'Centre', 'Department').
	subdivision('ht-ga', 'ht', 'Grande’Anse', 'Department').
	subdivision('ht-nd', 'ht', 'Nord', 'Department').
	subdivision('ht-ne', 'ht', 'Nord-Est', 'Department').
	subdivision('ht-ni', 'ht', 'Nippes', 'Department').
	subdivision('ht-no', 'ht', 'Nord-Ouest', 'Department').
	subdivision('ht-ou', 'ht', 'Ouest', 'Department').
	subdivision('ht-sd', 'ht', 'Sud', 'Department').
	subdivision('ht-se', 'ht', 'Sud-Est', 'Department').
	subdivision('hu-ba', 'hu', 'Baranya', 'County').
	subdivision('hu-bc', 'hu', 'Békéscsaba', 'City with county rights').
	subdivision('hu-be', 'hu', 'Békés', 'County').
	subdivision('hu-bk', 'hu', 'Bács-Kiskun', 'County').
	subdivision('hu-bu', 'hu', 'Budapest', 'Capital city').
	subdivision('hu-bz', 'hu', 'Borsod-Abaúj-Zemplén', 'County').
	subdivision('hu-cs', 'hu', 'Csongrád-Csanád', 'County').
	subdivision('hu-de', 'hu', 'Debrecen', 'City with county rights').
	subdivision('hu-du', 'hu', 'Dunaújváros', 'City with county rights').
	subdivision('hu-eg', 'hu', 'Eger', 'City with county rights').
	subdivision('hu-er', 'hu', 'Érd', 'City with county rights').
	subdivision('hu-fe', 'hu', 'Fejér', 'County').
	subdivision('hu-gs', 'hu', 'Győr-Moson-Sopron', 'County').
	subdivision('hu-gy', 'hu', 'Győr', 'City with county rights').
	subdivision('hu-hb', 'hu', 'Hajdú-Bihar', 'County').
	subdivision('hu-he', 'hu', 'Heves', 'County').
	subdivision('hu-hv', 'hu', 'Hódmezővásárhely', 'City with county rights').
	subdivision('hu-jn', 'hu', 'Jász-Nagykun-Szolnok', 'County').
	subdivision('hu-ke', 'hu', 'Komárom-Esztergom', 'County').
	subdivision('hu-km', 'hu', 'Kecskemét', 'City with county rights').
	subdivision('hu-kv', 'hu', 'Kaposvár', 'City with county rights').
	subdivision('hu-mi', 'hu', 'Miskolc', 'City with county rights').
	subdivision('hu-nk', 'hu', 'Nagykanizsa', 'City with county rights').
	subdivision('hu-no', 'hu', 'Nógrád', 'County').
	subdivision('hu-ny', 'hu', 'Nyíregyháza', 'City with county rights').
	subdivision('hu-pe', 'hu', 'Pest', 'County').
	subdivision('hu-ps', 'hu', 'Pécs', 'City with county rights').
	subdivision('hu-sd', 'hu', 'Szeged', 'City with county rights').
	subdivision('hu-sf', 'hu', 'Székesfehérvár', 'City with county rights').
	subdivision('hu-sh', 'hu', 'Szombathely', 'City with county rights').
	subdivision('hu-sk', 'hu', 'Szolnok', 'City with county rights').
	subdivision('hu-sn', 'hu', 'Sopron', 'City with county rights').
	subdivision('hu-so', 'hu', 'Somogy', 'County').
	subdivision('hu-ss', 'hu', 'Szekszárd', 'City with county rights').
	subdivision('hu-st', 'hu', 'Salgótarján', 'City with county rights').
	subdivision('hu-sz', 'hu', 'Szabolcs-Szatmár-Bereg', 'County').
	subdivision('hu-tb', 'hu', 'Tatabánya', 'City with county rights').
	subdivision('hu-to', 'hu', 'Tolna', 'County').
	subdivision('hu-va', 'hu', 'Vas', 'County').
	subdivision('hu-ve', 'hu', 'Veszprém', 'County').
	subdivision('hu-vm', 'hu', 'Veszprém', 'City with county rights').
	subdivision('hu-za', 'hu', 'Zala', 'County').
	subdivision('hu-ze', 'hu', 'Zalaegerszeg', 'City with county rights').
	subdivision('id-ac', 'id', 'Aceh', 'Province').
	subdivision('id-ba', 'id', 'Bali', 'Province').
	subdivision('id-bb', 'id', 'Kepulauan Bangka Belitung', 'Province').
	subdivision('id-be', 'id', 'Bengkulu', 'Province').
	subdivision('id-bt', 'id', 'Banten', 'Province').
	subdivision('id-go', 'id', 'Gorontalo', 'Province').
	subdivision('id-ja', 'id', 'Jambi', 'Province').
	subdivision('id-jb', 'id', 'Jawa Barat', 'Province').
	subdivision('id-ji', 'id', 'Jawa Timur', 'Province').
	subdivision('id-jk', 'id', 'Jakarta Raya', 'Capital district').
	subdivision('id-jt', 'id', 'Jawa Tengah', 'Province').
	subdivision('id-jw', 'id', 'Jawa', 'Geographical unit').
	subdivision('id-ka', 'id', 'Kalimantan', 'Geographical unit').
	subdivision('id-kb', 'id', 'Kalimantan Barat', 'Province').
	subdivision('id-ki', 'id', 'Kalimantan Timur', 'Province').
	subdivision('id-kr', 'id', 'Kepulauan Riau', 'Province').
	subdivision('id-ks', 'id', 'Kalimantan Selatan', 'Province').
	subdivision('id-kt', 'id', 'Kalimantan Tengah', 'Province').
	subdivision('id-ku', 'id', 'Kalimantan Utara', 'Province').
	subdivision('id-la', 'id', 'Lampung', 'Province').
	subdivision('id-ma', 'id', 'Maluku', 'Province').
	subdivision('id-ml', 'id', 'Maluku', 'Geographical unit').
	subdivision('id-mu', 'id', 'Maluku Utara', 'Province').
	subdivision('id-nb', 'id', 'Nusa Tenggara Barat', 'Province').
	subdivision('id-nt', 'id', 'Nusa Tenggara Timur', 'Province').
	subdivision('id-nu', 'id', 'Nusa Tenggara', 'Geographical unit').
	subdivision('id-pa', 'id', 'Papua', 'Province').
	subdivision('id-pb', 'id', 'Papua Barat', 'Province').
	subdivision('id-pd', 'id', 'Papua Barat Daya', 'Province').
	subdivision('id-pe', 'id', 'Papua Pengunungan', 'Province').
	subdivision('id-pp', 'id', 'Papua', 'Geographical unit').
	subdivision('id-ps', 'id', 'Papua Selatan', 'Province').
	subdivision('id-pt', 'id', 'Papua Tengah', 'Province').
	subdivision('id-ri', 'id', 'Riau', 'Province').
	subdivision('id-sa', 'id', 'Sulawesi Utara', 'Province').
	subdivision('id-sb', 'id', 'Sumatera Barat', 'Province').
	subdivision('id-sg', 'id', 'Sulawesi Tenggara', 'Province').
	subdivision('id-sl', 'id', 'Sulawesi', 'Geographical unit').
	subdivision('id-sm', 'id', 'Sumatera', 'Geographical unit').
	subdivision('id-sn', 'id', 'Sulawesi Selatan', 'Province').
	subdivision('id-sr', 'id', 'Sulawesi Barat', 'Province').
	subdivision('id-ss', 'id', 'Sumatera Selatan', 'Province').
	subdivision('id-st', 'id', 'Sulawesi Tengah', 'Province').
	subdivision('id-su', 'id', 'Sumatera Utara', 'Province').
	subdivision('id-yo', 'id', 'Yogyakarta', 'Special region').
	subdivision('ie-c', 'ie', 'Connaught', 'Province').
	subdivision('ie-ce', 'ie', 'Clare', 'County').
	subdivision('ie-cn', 'ie', 'Cavan', 'County').
	subdivision('ie-co', 'ie', 'Cork', 'County').
	subdivision('ie-cw', 'ie', 'Carlow', 'County').
	subdivision('ie-d', 'ie', 'Dublin', 'County').
	subdivision('ie-dl', 'ie', 'Donegal', 'County').
	subdivision('ie-g', 'ie', 'Galway', 'County').
	subdivision('ie-ke', 'ie', 'Kildare', 'County').
	subdivision('ie-kk', 'ie', 'Kilkenny', 'County').
	subdivision('ie-ky', 'ie', 'Kerry', 'County').
	subdivision('ie-l', 'ie', 'Leinster', 'Province').
	subdivision('ie-ld', 'ie', 'Longford', 'County').
	subdivision('ie-lh', 'ie', 'Louth', 'County').
	subdivision('ie-lk', 'ie', 'Limerick', 'County').
	subdivision('ie-lm', 'ie', 'Leitrim', 'County').
	subdivision('ie-ls', 'ie', 'Laois', 'County').
	subdivision('ie-m', 'ie', 'Munster', 'Province').
	subdivision('ie-mh', 'ie', 'Meath', 'County').
	subdivision('ie-mn', 'ie', 'Monaghan', 'County').
	subdivision('ie-mo', 'ie', 'Mayo', 'County').
	subdivision('ie-oy', 'ie', 'Offaly', 'County').
	subdivision('ie-rn', 'ie', 'Roscommon', 'County').
	subdivision('ie-so', 'ie', 'Sligo', 'County').
	subdivision('ie-ta', 'ie', 'Tipperary', 'County').
	subdivision('ie-u', 'ie', 'Ulster', 'Province').
	subdivision('ie-wd', 'ie', 'Waterford', 'County').
	subdivision('ie-wh', 'ie', 'Westmeath', 'County').
	subdivision('ie-ww', 'ie', 'Wicklow', 'County').
	subdivision('ie-wx', 'ie', 'Wexford', 'County').
	subdivision('il-d', 'il', 'Al Janūbī', 'District').
	subdivision('il-ha', 'il', 'Ḩayfā', 'District').
	subdivision('il-jm', 'il', 'Al Quds', 'District').
	subdivision('il-m', 'il', 'Al Awsaţ', 'District').
	subdivision('il-ta', 'il', 'Tall Abīb', 'District').
	subdivision('il-z', 'il', 'Ash Shamālī', 'District').
	subdivision('in-an', 'in', 'Andaman and Nicobar Islands', 'Union territory').
	subdivision('in-ap', 'in', 'Andhra Pradesh', 'State').
	subdivision('in-ar', 'in', 'Arunāchal Pradesh', 'State').
	subdivision('in-as', 'in', 'Assam', 'State').
	subdivision('in-br', 'in', 'Bihār', 'State').
	subdivision('in-cg', 'in', 'Chhattīsgarh', 'State').
	subdivision('in-ch', 'in', 'Chandīgarh', 'Union territory').
	subdivision('in-dh', 'in', 'Dādra and Nagar Haveli and Damān and Diu', 'Union territory').
	subdivision('in-dl', 'in', 'Delhi', 'Union territory').
	subdivision('in-ga', 'in', 'Goa', 'State').
	subdivision('in-gj', 'in', 'Gujarāt', 'State').
	subdivision('in-hp', 'in', 'Himāchal Pradesh', 'State').
	subdivision('in-hr', 'in', 'Haryāna', 'State').
	subdivision('in-jh', 'in', 'Jhārkhand', 'State').
	subdivision('in-jk', 'in', 'Jammu and Kashmīr', 'Union territory').
	subdivision('in-ka', 'in', 'Karnātaka', 'State').
	subdivision('in-kl', 'in', 'Kerala', 'State').
	subdivision('in-la', 'in', 'Ladākh', 'Union territory').
	subdivision('in-ld', 'in', 'Lakshadweep', 'Union territory').
	subdivision('in-mh', 'in', 'Mahārāshtra', 'State').
	subdivision('in-ml', 'in', 'Meghālaya', 'State').
	subdivision('in-mn', 'in', 'Manipur', 'State').
	subdivision('in-mp', 'in', 'Madhya Pradesh', 'State').
	subdivision('in-mz', 'in', 'Mizoram', 'State').
	subdivision('in-nl', 'in', 'Nāgāland', 'State').
	subdivision('in-od', 'in', 'Odisha', 'State').
	subdivision('in-pb', 'in', 'Punjab', 'State').
	subdivision('in-py', 'in', 'Puducherry', 'Union territory').
	subdivision('in-rj', 'in', 'Rājasthān', 'State').
	subdivision('in-sk', 'in', 'Sikkim', 'State').
	subdivision('in-tn', 'in', 'Tamil Nādu', 'State').
	subdivision('in-tr', 'in', 'Tripura', 'State').
	subdivision('in-ts', 'in', 'Telangāna', 'State').
	subdivision('in-uk', 'in', 'Uttarākhand', 'State').
	subdivision('in-up', 'in', 'Uttar Pradesh', 'State').
	subdivision('in-wb', 'in', 'West Bengal', 'State').
	subdivision('iq-an', 'iq', 'Al Anbār', 'Governorate').
	subdivision('iq-ar', 'iq', 'Arbīl', 'Governorate').
	subdivision('iq-ba', 'iq', 'Al Başrah', 'Governorate').
	subdivision('iq-bb', 'iq', 'Bābil', 'Governorate').
	subdivision('iq-bg', 'iq', 'Baghdād', 'Governorate').
	subdivision('iq-da', 'iq', 'Dahūk', 'Governorate').
	subdivision('iq-di', 'iq', 'Diyālá', 'Governorate').
	subdivision('iq-dq', 'iq', 'Dhī Qār', 'Governorate').
	subdivision('iq-ka', 'iq', 'Karbalā’', 'Governorate').
	subdivision('iq-ki', 'iq', 'Kirkūk', 'Governorate').
	subdivision('iq-kr', 'iq', 'Iqlīm Kūrdistān', 'Region').
	subdivision('iq-ma', 'iq', 'Maysān', 'Governorate').
	subdivision('iq-mu', 'iq', 'Al Muthanná', 'Governorate').
	subdivision('iq-na', 'iq', 'An Najaf', 'Governorate').
	subdivision('iq-ni', 'iq', 'Nīnawá', 'Governorate').
	subdivision('iq-qa', 'iq', 'Al Qādisīyah', 'Governorate').
	subdivision('iq-sd', 'iq', 'Şalāḩ ad Dīn', 'Governorate').
	subdivision('iq-su', 'iq', 'As Sulaymānīyah', 'Governorate').
	subdivision('iq-wa', 'iq', 'Wāsiţ', 'Governorate').
	subdivision('ir-00', 'ir', 'Markazī', 'Province').
	subdivision('ir-01', 'ir', 'Gīlān', 'Province').
	subdivision('ir-02', 'ir', 'Māzandarān', 'Province').
	subdivision('ir-03', 'ir', 'Āz̄ārbāyjān-e Shārqī', 'Province').
	subdivision('ir-04', 'ir', 'Āz̄ārbāyjān-e Ghārbī', 'Province').
	subdivision('ir-05', 'ir', 'Kermānshāh', 'Province').
	subdivision('ir-06', 'ir', 'Khūzestān', 'Province').
	subdivision('ir-07', 'ir', 'Fārs', 'Province').
	subdivision('ir-08', 'ir', 'Kermān', 'Province').
	subdivision('ir-09', 'ir', 'Khorāsān-e Raẕavī', 'Province').
	subdivision('ir-10', 'ir', 'Eşfahān', 'Province').
	subdivision('ir-11', 'ir', 'Sīstān va Balūchestān', 'Province').
	subdivision('ir-12', 'ir', 'Kordestān', 'Province').
	subdivision('ir-13', 'ir', 'Hamadān', 'Province').
	subdivision('ir-14', 'ir', 'Chahār Maḩāl va Bakhtīārī', 'Province').
	subdivision('ir-15', 'ir', 'Lorestān', 'Province').
	subdivision('ir-16', 'ir', 'Īlām', 'Province').
	subdivision('ir-17', 'ir', 'Kohgīlūyeh va Bowyer Aḩmad', 'Province').
	subdivision('ir-18', 'ir', 'Būshehr', 'Province').
	subdivision('ir-19', 'ir', 'Zanjān', 'Province').
	subdivision('ir-20', 'ir', 'Semnān', 'Province').
	subdivision('ir-21', 'ir', 'Yazd', 'Province').
	subdivision('ir-22', 'ir', 'Hormozgān', 'Province').
	subdivision('ir-23', 'ir', 'Tehrān', 'Province').
	subdivision('ir-24', 'ir', 'Ardabīl', 'Province').
	subdivision('ir-25', 'ir', 'Qom', 'Province').
	subdivision('ir-26', 'ir', 'Qazvīn', 'Province').
	subdivision('ir-27', 'ir', 'Golestān', 'Province').
	subdivision('ir-28', 'ir', 'Khorāsān-e Shomālī', 'Province').
	subdivision('ir-29', 'ir', 'Khorāsān-e Jonūbī', 'Province').
	subdivision('ir-30', 'ir', 'Alborz', 'Province').
	subdivision('is-1', 'is', 'Höfuðborgarsvæði', 'Region').
	subdivision('is-2', 'is', 'Suðurnes', 'Region').
	subdivision('is-3', 'is', 'Vesturland', 'Region').
	subdivision('is-4', 'is', 'Vestfirðir', 'Region').
	subdivision('is-5', 'is', 'Norðurland vestra', 'Region').
	subdivision('is-6', 'is', 'Norðurland eystra', 'Region').
	subdivision('is-7', 'is', 'Austurland', 'Region').
	subdivision('is-8', 'is', 'Suðurland', 'Region').
	subdivision('is-akn', 'is', 'Akraneskaupstaður', 'Municipality').
	subdivision('is-aku', 'is', 'Akureyrarbær', 'Municipality').
	subdivision('is-arn', 'is', 'Árneshreppur', 'Municipality').
	subdivision('is-asa', 'is', 'Ásahreppur', 'Municipality').
	subdivision('is-bla', 'is', 'Bláskógabyggð', 'Municipality').
	subdivision('is-bog', 'is', 'Borgarbyggð', 'Municipality').
	subdivision('is-bol', 'is', 'Bolungarvíkurkaupstaður', 'Municipality').
	subdivision('is-dab', 'is', 'Dalabyggð', 'Municipality').
	subdivision('is-dav', 'is', 'Dalvíkurbyggð', 'Municipality').
	subdivision('is-eom', 'is', 'Eyja- og Miklaholtshreppur', 'Municipality').
	subdivision('is-eyf', 'is', 'Eyjafjarðarsveit', 'Municipality').
	subdivision('is-fjd', 'is', 'Fjarðabyggð', 'Municipality').
	subdivision('is-fjl', 'is', 'Fjallabyggð', 'Municipality').
	subdivision('is-fla', 'is', 'Flóahreppur', 'Municipality').
	subdivision('is-flr', 'is', 'Fljótsdalshreppur', 'Municipality').
	subdivision('is-gar', 'is', 'Garðabær', 'Municipality').
	subdivision('is-gog', 'is', 'Grímsnes- og Grafningshreppur', 'Municipality').
	subdivision('is-grn', 'is', 'Grindavíkurbær', 'Municipality').
	subdivision('is-gru', 'is', 'Grundarfjarðarbær', 'Municipality').
	subdivision('is-gry', 'is', 'Grýtubakkahreppur', 'Municipality').
	subdivision('is-haf', 'is', 'Hafnarfjarðarkaupstaður', 'Municipality').
	subdivision('is-hrg', 'is', 'Hörgársveit', 'Municipality').
	subdivision('is-hru', 'is', 'Hrunamannahreppur', 'Municipality').
	subdivision('is-hug', 'is', 'Húnabyggð', 'Municipality').
	subdivision('is-huv', 'is', 'Húnaþing vestra', 'Municipality').
	subdivision('is-hva', 'is', 'Hvalfjarðarsveit', 'Municipality').
	subdivision('is-hve', 'is', 'Hveragerðisbær', 'Municipality').
	subdivision('is-isa', 'is', 'Ísafjarðarbær', 'Municipality').
	subdivision('is-kal', 'is', 'Kaldrananeshreppur', 'Municipality').
	subdivision('is-kjo', 'is', 'Kjósarhreppur', 'Municipality').
	subdivision('is-kop', 'is', 'Kópavogsbær', 'Municipality').
	subdivision('is-lan', 'is', 'Langanesbyggð', 'Municipality').
	subdivision('is-mos', 'is', 'Mosfellsbær', 'Municipality').
	subdivision('is-mul', 'is', 'Múlaþing', 'Municipality').
	subdivision('is-myr', 'is', 'Mýrdalshreppur', 'Municipality').
	subdivision('is-nor', 'is', 'Norðurþing', 'Municipality').
	subdivision('is-rge', 'is', 'Rangárþing eystra', 'Municipality').
	subdivision('is-rgy', 'is', 'Rangárþing ytra', 'Municipality').
	subdivision('is-rhh', 'is', 'Reykhólahreppur', 'Municipality').
	subdivision('is-rkn', 'is', 'Reykjanesbær', 'Municipality').
	subdivision('is-rkv', 'is', 'Reykjavíkurborg', 'Municipality').
	subdivision('is-sbt', 'is', 'Svalbarðsstrandarhreppur', 'Municipality').
	subdivision('is-sdn', 'is', 'Suðurnesjabær', 'Municipality').
	subdivision('is-sdv', 'is', 'Súðavíkurhreppur', 'Municipality').
	subdivision('is-sel', 'is', 'Seltjarnarnesbær', 'Municipality').
	subdivision('is-sfa', 'is', 'Sveitarfélagið Árborg', 'Municipality').
	subdivision('is-shf', 'is', 'Sveitarfélagið Hornafjörður', 'Municipality').
	subdivision('is-skf', 'is', 'Skaftárhreppur', 'Municipality').
	subdivision('is-skg', 'is', 'Skagabyggð', 'Municipality').
	subdivision('is-sko', 'is', 'Skorradalshreppur', 'Municipality').
	subdivision('is-skr', 'is', 'Skagafjörður', 'Municipality').
	subdivision('is-snf', 'is', 'Snæfellsbær', 'Municipality').
	subdivision('is-sog', 'is', 'Skeiða- og Gnúpverjahreppur', 'Municipality').
	subdivision('is-sol', 'is', 'Sveitarfélagið Ölfus', 'Municipality').
	subdivision('is-sss', 'is', 'Sveitarfélagið Skagaströnd', 'Municipality').
	subdivision('is-str', 'is', 'Strandabyggð', 'Municipality').
	subdivision('is-sty', 'is', 'Stykkishólmsbær', 'Municipality').
	subdivision('is-svg', 'is', 'Sveitarfélagið Vogar', 'Municipality').
	subdivision('is-tal', 'is', 'Tálknafjarðarhreppur', 'Municipality').
	subdivision('is-thg', 'is', 'Þingeyjarsveit', 'Municipality').
	subdivision('is-tjo', 'is', 'Tjörneshreppur', 'Municipality').
	subdivision('is-vem', 'is', 'Vestmannaeyjabær', 'Municipality').
	subdivision('is-ver', 'is', 'Vesturbyggð', 'Municipality').
	subdivision('is-vop', 'is', 'Vopnafjarðarhreppur', 'Municipality').
	subdivision('it-21', 'it', 'Piemonte', 'Region').
	subdivision('it-23', 'it', 'Valle d''Aosta', 'Autonomous region').
	subdivision('it-25', 'it', 'Lombardia', 'Region').
	subdivision('it-32', 'it', 'Trentino-Alto Adige', 'Autonomous region').
	subdivision('it-34', 'it', 'Veneto', 'Region').
	subdivision('it-36', 'it', 'Friuli Venezia Giulia', 'Autonomous region').
	subdivision('it-42', 'it', 'Liguria', 'Region').
	subdivision('it-45', 'it', 'Emilia-Romagna', 'Region').
	subdivision('it-52', 'it', 'Toscana', 'Region').
	subdivision('it-55', 'it', 'Umbria', 'Region').
	subdivision('it-57', 'it', 'Marche', 'Region').
	subdivision('it-62', 'it', 'Lazio', 'Region').
	subdivision('it-65', 'it', 'Abruzzo', 'Region').
	subdivision('it-67', 'it', 'Molise', 'Region').
	subdivision('it-72', 'it', 'Campania', 'Region').
	subdivision('it-75', 'it', 'Puglia', 'Region').
	subdivision('it-77', 'it', 'Basilicata', 'Region').
	subdivision('it-78', 'it', 'Calabria', 'Region').
	subdivision('it-82', 'it', 'Sicilia', 'Autonomous region').
	subdivision('it-88', 'it', 'Sardegna', 'Autonomous region').
	subdivision('it-ag', 'it', 'Agrigento', 'Free municipal consortium').
	subdivision('it-al', 'it', 'Alessandria', 'Province').
	subdivision('it-an', 'it', 'Ancona', 'Province').
	subdivision('it-ap', 'it', 'Ascoli Piceno', 'Province').
	subdivision('it-aq', 'it', 'L''Aquila', 'Province').
	subdivision('it-ar', 'it', 'Arezzo', 'Province').
	subdivision('it-at', 'it', 'Asti', 'Province').
	subdivision('it-av', 'it', 'Avellino', 'Province').
	subdivision('it-ba', 'it', 'Bari', 'Metropolitan city').
	subdivision('it-bg', 'it', 'Bergamo', 'Province').
	subdivision('it-bi', 'it', 'Biella', 'Province').
	subdivision('it-bl', 'it', 'Belluno', 'Province').
	subdivision('it-bn', 'it', 'Benevento', 'Province').
	subdivision('it-bo', 'it', 'Bologna', 'Metropolitan city').
	subdivision('it-br', 'it', 'Brindisi', 'Province').
	subdivision('it-bs', 'it', 'Brescia', 'Province').
	subdivision('it-bt', 'it', 'Barletta-Andria-Trani', 'Province').
	subdivision('it-bz', 'it', 'Bolzano', 'Autonomous province').
	subdivision('it-ca', 'it', 'Cagliari', 'Metropolitan city').
	subdivision('it-cb', 'it', 'Campobasso', 'Province').
	subdivision('it-ce', 'it', 'Caserta', 'Province').
	subdivision('it-ch', 'it', 'Chieti', 'Province').
	subdivision('it-cl', 'it', 'Caltanissetta', 'Free municipal consortium').
	subdivision('it-cn', 'it', 'Cuneo', 'Province').
	subdivision('it-co', 'it', 'Como', 'Province').
	subdivision('it-cr', 'it', 'Cremona', 'Province').
	subdivision('it-cs', 'it', 'Cosenza', 'Province').
	subdivision('it-ct', 'it', 'Catania', 'Metropolitan city').
	subdivision('it-cz', 'it', 'Catanzaro', 'Province').
	subdivision('it-en', 'it', 'Enna', 'Free municipal consortium').
	subdivision('it-fc', 'it', 'Forlì-Cesena', 'Province').
	subdivision('it-fe', 'it', 'Ferrara', 'Province').
	subdivision('it-fg', 'it', 'Foggia', 'Province').
	subdivision('it-fi', 'it', 'Firenze', 'Metropolitan city').
	subdivision('it-fm', 'it', 'Fermo', 'Province').
	subdivision('it-fr', 'it', 'Frosinone', 'Province').
	subdivision('it-ge', 'it', 'Genova', 'Metropolitan city').
	subdivision('it-go', 'it', 'Gorizia', 'Decentralized regional entity').
	subdivision('it-gr', 'it', 'Grosseto', 'Province').
	subdivision('it-im', 'it', 'Imperia', 'Province').
	subdivision('it-is', 'it', 'Isernia', 'Province').
	subdivision('it-kr', 'it', 'Crotone', 'Province').
	subdivision('it-lc', 'it', 'Lecco', 'Province').
	subdivision('it-le', 'it', 'Lecce', 'Province').
	subdivision('it-li', 'it', 'Livorno', 'Province').
	subdivision('it-lo', 'it', 'Lodi', 'Province').
	subdivision('it-lt', 'it', 'Latina', 'Province').
	subdivision('it-lu', 'it', 'Lucca', 'Province').
	subdivision('it-mb', 'it', 'Monza e Brianza', 'Province').
	subdivision('it-mc', 'it', 'Macerata', 'Province').
	subdivision('it-me', 'it', 'Messina', 'Metropolitan city').
	subdivision('it-mi', 'it', 'Milano', 'Metropolitan city').
	subdivision('it-mn', 'it', 'Mantova', 'Province').
	subdivision('it-mo', 'it', 'Modena', 'Province').
	subdivision('it-ms', 'it', 'Massa-Carrara', 'Province').
	subdivision('it-mt', 'it', 'Matera', 'Province').
	subdivision('it-na', 'it', 'Napoli', 'Metropolitan city').
	subdivision('it-no', 'it', 'Novara', 'Province').
	subdivision('it-nu', 'it', 'Nuoro', 'Province').
	subdivision('it-or', 'it', 'Oristano', 'Province').
	subdivision('it-pa', 'it', 'Palermo', 'Metropolitan city').
	subdivision('it-pc', 'it', 'Piacenza', 'Province').
	subdivision('it-pd', 'it', 'Padova', 'Province').
	subdivision('it-pe', 'it', 'Pescara', 'Province').
	subdivision('it-pg', 'it', 'Perugia', 'Province').
	subdivision('it-pi', 'it', 'Pisa', 'Province').
	subdivision('it-pn', 'it', 'Pordenone', 'Decentralized regional entity').
	subdivision('it-po', 'it', 'Prato', 'Province').
	subdivision('it-pr', 'it', 'Parma', 'Province').
	subdivision('it-pt', 'it', 'Pistoia', 'Province').
	subdivision('it-pu', 'it', 'Pesaro e Urbino', 'Province').
	subdivision('it-pv', 'it', 'Pavia', 'Province').
	subdivision('it-pz', 'it', 'Potenza', 'Province').
	subdivision('it-ra', 'it', 'Ravenna', 'Province').
	subdivision('it-rc', 'it', 'Reggio Calabria', 'Metropolitan city').
	subdivision('it-re', 'it', 'Reggio Emilia', 'Province').
	subdivision('it-rg', 'it', 'Ragusa', 'Free municipal consortium').
	subdivision('it-ri', 'it', 'Rieti', 'Province').
	subdivision('it-rm', 'it', 'Roma', 'Metropolitan city').
	subdivision('it-rn', 'it', 'Rimini', 'Province').
	subdivision('it-ro', 'it', 'Rovigo', 'Province').
	subdivision('it-sa', 'it', 'Salerno', 'Province').
	subdivision('it-si', 'it', 'Siena', 'Province').
	subdivision('it-so', 'it', 'Sondrio', 'Province').
	subdivision('it-sp', 'it', 'La Spezia', 'Province').
	subdivision('it-sr', 'it', 'Siracusa', 'Free municipal consortium').
	subdivision('it-ss', 'it', 'Sassari', 'Province').
	subdivision('it-su', 'it', 'Sud Sardegna', 'Province').
	subdivision('it-sv', 'it', 'Savona', 'Province').
	subdivision('it-ta', 'it', 'Taranto', 'Province').
	subdivision('it-te', 'it', 'Teramo', 'Province').
	subdivision('it-tn', 'it', 'Trento', 'Autonomous province').
	subdivision('it-to', 'it', 'Torino', 'Metropolitan city').
	subdivision('it-tp', 'it', 'Trapani', 'Free municipal consortium').
	subdivision('it-tr', 'it', 'Terni', 'Province').
	subdivision('it-ts', 'it', 'Trieste', 'Decentralized regional entity').
	subdivision('it-tv', 'it', 'Treviso', 'Province').
	subdivision('it-ud', 'it', 'Udine', 'Decentralized regional entity').
	subdivision('it-va', 'it', 'Varese', 'Province').
	subdivision('it-vb', 'it', 'Verbano-Cusio-Ossola', 'Province').
	subdivision('it-vc', 'it', 'Vercelli', 'Province').
	subdivision('it-ve', 'it', 'Venezia', 'Metropolitan city').
	subdivision('it-vi', 'it', 'Vicenza', 'Province').
	subdivision('it-vr', 'it', 'Verona', 'Province').
	subdivision('it-vt', 'it', 'Viterbo', 'Province').
	subdivision('it-vv', 'it', 'Vibo Valentia', 'Province').
	subdivision('jm-01', 'jm', 'Kingston', 'Parish').
	subdivision('jm-02', 'jm', 'Saint Andrew', 'Parish').
	subdivision('jm-03', 'jm', 'Saint Thomas', 'Parish').
	subdivision('jm-04', 'jm', 'Portland', 'Parish').
	subdivision('jm-05', 'jm', 'Saint Mary', 'Parish').
	subdivision('jm-06', 'jm', 'Saint Ann', 'Parish').
	subdivision('jm-07', 'jm', 'Trelawny', 'Parish').
	subdivision('jm-08', 'jm', 'Saint James', 'Parish').
	subdivision('jm-09', 'jm', 'Hanover', 'Parish').
	subdivision('jm-10', 'jm', 'Westmoreland', 'Parish').
	subdivision('jm-11', 'jm', 'Saint Elizabeth', 'Parish').
	subdivision('jm-12', 'jm', 'Manchester', 'Parish').
	subdivision('jm-13', 'jm', 'Clarendon', 'Parish').
	subdivision('jm-14', 'jm', 'Saint Catherine', 'Parish').
	subdivision('jo-aj', 'jo', '‘Ajlūn', 'Governorate').
	subdivision('jo-am', 'jo', 'Al ‘A̅şimah', 'Governorate').
	subdivision('jo-aq', 'jo', 'Al ‘Aqabah', 'Governorate').
	subdivision('jo-at', 'jo', 'Aţ Ţafīlah', 'Governorate').
	subdivision('jo-az', 'jo', 'Az Zarqā’', 'Governorate').
	subdivision('jo-ba', 'jo', 'Al Balqā’', 'Governorate').
	subdivision('jo-ir', 'jo', 'Irbid', 'Governorate').
	subdivision('jo-ja', 'jo', 'Jarash', 'Governorate').
	subdivision('jo-ka', 'jo', 'Al Karak', 'Governorate').
	subdivision('jo-ma', 'jo', 'Al Mafraq', 'Governorate').
	subdivision('jo-md', 'jo', 'Mādabā', 'Governorate').
	subdivision('jo-mn', 'jo', 'Ma‘ān', 'Governorate').
	subdivision('jp-01', 'jp', 'Hokkaido', 'Prefecture').
	subdivision('jp-02', 'jp', 'Aomori', 'Prefecture').
	subdivision('jp-03', 'jp', 'Iwate', 'Prefecture').
	subdivision('jp-04', 'jp', 'Miyagi', 'Prefecture').
	subdivision('jp-05', 'jp', 'Akita', 'Prefecture').
	subdivision('jp-06', 'jp', 'Yamagata', 'Prefecture').
	subdivision('jp-07', 'jp', 'Fukushima', 'Prefecture').
	subdivision('jp-08', 'jp', 'Ibaraki', 'Prefecture').
	subdivision('jp-09', 'jp', 'Tochigi', 'Prefecture').
	subdivision('jp-10', 'jp', 'Gunma', 'Prefecture').
	subdivision('jp-11', 'jp', 'Saitama', 'Prefecture').
	subdivision('jp-12', 'jp', 'Chiba', 'Prefecture').
	subdivision('jp-13', 'jp', 'Tokyo', 'Prefecture').
	subdivision('jp-14', 'jp', 'Kanagawa', 'Prefecture').
	subdivision('jp-15', 'jp', 'Niigata', 'Prefecture').
	subdivision('jp-16', 'jp', 'Toyama', 'Prefecture').
	subdivision('jp-17', 'jp', 'Ishikawa', 'Prefecture').
	subdivision('jp-18', 'jp', 'Fukui', 'Prefecture').
	subdivision('jp-19', 'jp', 'Yamanashi', 'Prefecture').
	subdivision('jp-20', 'jp', 'Nagano', 'Prefecture').
	subdivision('jp-21', 'jp', 'Gifu', 'Prefecture').
	subdivision('jp-22', 'jp', 'Shizuoka', 'Prefecture').
	subdivision('jp-23', 'jp', 'Aichi', 'Prefecture').
	subdivision('jp-24', 'jp', 'Mie', 'Prefecture').
	subdivision('jp-25', 'jp', 'Shiga', 'Prefecture').
	subdivision('jp-26', 'jp', 'Kyoto', 'Prefecture').
	subdivision('jp-27', 'jp', 'Osaka', 'Prefecture').
	subdivision('jp-28', 'jp', 'Hyogo', 'Prefecture').
	subdivision('jp-29', 'jp', 'Nara', 'Prefecture').
	subdivision('jp-30', 'jp', 'Wakayama', 'Prefecture').
	subdivision('jp-31', 'jp', 'Tottori', 'Prefecture').
	subdivision('jp-32', 'jp', 'Shimane', 'Prefecture').
	subdivision('jp-33', 'jp', 'Okayama', 'Prefecture').
	subdivision('jp-34', 'jp', 'Hiroshima', 'Prefecture').
	subdivision('jp-35', 'jp', 'Yamaguchi', 'Prefecture').
	subdivision('jp-36', 'jp', 'Tokushima', 'Prefecture').
	subdivision('jp-37', 'jp', 'Kagawa', 'Prefecture').
	subdivision('jp-38', 'jp', 'Ehime', 'Prefecture').
	subdivision('jp-39', 'jp', 'Kochi', 'Prefecture').
	subdivision('jp-40', 'jp', 'Fukuoka', 'Prefecture').
	subdivision('jp-41', 'jp', 'Saga', 'Prefecture').
	subdivision('jp-42', 'jp', 'Nagasaki', 'Prefecture').
	subdivision('jp-43', 'jp', 'Kumamoto', 'Prefecture').
	subdivision('jp-44', 'jp', 'Oita', 'Prefecture').
	subdivision('jp-45', 'jp', 'Miyazaki', 'Prefecture').
	subdivision('jp-46', 'jp', 'Kagoshima', 'Prefecture').
	subdivision('jp-47', 'jp', 'Okinawa', 'Prefecture').
	subdivision('ke-01', 'ke', 'Baringo', 'County').
	subdivision('ke-02', 'ke', 'Bomet', 'County').
	subdivision('ke-03', 'ke', 'Bungoma', 'County').
	subdivision('ke-04', 'ke', 'Busia', 'County').
	subdivision('ke-05', 'ke', 'Elgeyo/Marakwet', 'County').
	subdivision('ke-06', 'ke', 'Embu', 'County').
	subdivision('ke-07', 'ke', 'Garissa', 'County').
	subdivision('ke-08', 'ke', 'Homa Bay', 'County').
	subdivision('ke-09', 'ke', 'Isiolo', 'County').
	subdivision('ke-10', 'ke', 'Kajiado', 'County').
	subdivision('ke-11', 'ke', 'Kakamega', 'County').
	subdivision('ke-12', 'ke', 'Kericho', 'County').
	subdivision('ke-13', 'ke', 'Kiambu', 'County').
	subdivision('ke-14', 'ke', 'Kilifi', 'County').
	subdivision('ke-15', 'ke', 'Kirinyaga', 'County').
	subdivision('ke-16', 'ke', 'Kisii', 'County').
	subdivision('ke-17', 'ke', 'Kisumu', 'County').
	subdivision('ke-18', 'ke', 'Kitui', 'County').
	subdivision('ke-19', 'ke', 'Kwale', 'County').
	subdivision('ke-20', 'ke', 'Laikipia', 'County').
	subdivision('ke-21', 'ke', 'Lamu', 'County').
	subdivision('ke-22', 'ke', 'Machakos', 'County').
	subdivision('ke-23', 'ke', 'Makueni', 'County').
	subdivision('ke-24', 'ke', 'Mandera', 'County').
	subdivision('ke-25', 'ke', 'Marsabit', 'County').
	subdivision('ke-26', 'ke', 'Meru', 'County').
	subdivision('ke-27', 'ke', 'Migori', 'County').
	subdivision('ke-28', 'ke', 'Mombasa', 'County').
	subdivision('ke-29', 'ke', 'Murang''a', 'County').
	subdivision('ke-30', 'ke', 'Nairobi City', 'County').
	subdivision('ke-31', 'ke', 'Nakuru', 'County').
	subdivision('ke-32', 'ke', 'Nandi', 'County').
	subdivision('ke-33', 'ke', 'Narok', 'County').
	subdivision('ke-34', 'ke', 'Nyamira', 'County').
	subdivision('ke-35', 'ke', 'Nyandarua', 'County').
	subdivision('ke-36', 'ke', 'Nyeri', 'County').
	subdivision('ke-37', 'ke', 'Samburu', 'County').
	subdivision('ke-38', 'ke', 'Siaya', 'County').
	subdivision('ke-39', 'ke', 'Taita/Taveta', 'County').
	subdivision('ke-40', 'ke', 'Tana River', 'County').
	subdivision('ke-41', 'ke', 'Tharaka-Nithi', 'County').
	subdivision('ke-42', 'ke', 'Trans Nzoia', 'County').
	subdivision('ke-43', 'ke', 'Turkana', 'County').
	subdivision('ke-44', 'ke', 'Uasin Gishu', 'County').
	subdivision('ke-45', 'ke', 'Vihiga', 'County').
	subdivision('ke-46', 'ke', 'Wajir', 'County').
	subdivision('ke-47', 'ke', 'West Pokot', 'County').
	subdivision('kg-b', 'kg', 'Batken', 'Region').
	subdivision('kg-c', 'kg', 'Chüy', 'Region').
	subdivision('kg-gb', 'kg', 'Bishkek Shaary', 'City').
	subdivision('kg-go', 'kg', 'Osh Shaary', 'City').
	subdivision('kg-j', 'kg', 'Jalal-Abad', 'Region').
	subdivision('kg-n', 'kg', 'Naryn', 'Region').
	subdivision('kg-o', 'kg', 'Osh', 'Region').
	subdivision('kg-t', 'kg', 'Talas', 'Region').
	subdivision('kg-y', 'kg', 'Ysyk-Köl', 'Region').
	subdivision('kh-1', 'kh', 'Banteay Mean Choăy', 'Province').
	subdivision('kh-10', 'kh', 'Kracheh', 'Province').
	subdivision('kh-11', 'kh', 'Mondol Kiri', 'Province').
	subdivision('kh-12', 'kh', 'Phnom Penh', 'Autonomous municipality').
	subdivision('kh-13', 'kh', 'Preah Vihear', 'Province').
	subdivision('kh-14', 'kh', 'Prey Veaeng', 'Province').
	subdivision('kh-15', 'kh', 'Pousaat', 'Province').
	subdivision('kh-16', 'kh', 'Rotanak Kiri', 'Province').
	subdivision('kh-17', 'kh', 'Siem Reab', 'Province').
	subdivision('kh-18', 'kh', 'Preah Sihanouk', 'Province').
	subdivision('kh-19', 'kh', 'Stueng Traeng', 'Province').
	subdivision('kh-2', 'kh', 'Baat Dambang', 'Province').
	subdivision('kh-20', 'kh', 'Svaay Rieng', 'Province').
	subdivision('kh-21', 'kh', 'Taakaev', 'Province').
	subdivision('kh-22', 'kh', 'Otdar Mean Chey', 'Province').
	subdivision('kh-23', 'kh', 'Kaeb', 'Province').
	subdivision('kh-24', 'kh', 'Pailin', 'Province').
	subdivision('kh-25', 'kh', 'Tbong Khmum', 'Province').
	subdivision('kh-3', 'kh', 'Kampong Chaam', 'Province').
	subdivision('kh-4', 'kh', 'Kampong Chhnang', 'Province').
	subdivision('kh-5', 'kh', 'Kampong Spueu', 'Province').
	subdivision('kh-6', 'kh', 'Kampong Thum', 'Province').
	subdivision('kh-7', 'kh', 'Kampot', 'Province').
	subdivision('kh-8', 'kh', 'Kandaal', 'Province').
	subdivision('kh-9', 'kh', 'Kaoh Kong', 'Province').
	subdivision('ki-g', 'ki', 'Gilbert Islands', 'Group of islands (20 inhabited islands)').
	subdivision('ki-l', 'ki', 'Line Islands', 'Group of islands (20 inhabited islands)').
	subdivision('ki-p', 'ki', 'Phoenix Islands', 'Group of islands (20 inhabited islands)').
	subdivision('km-a', 'km', 'Anjouan', 'Island').
	subdivision('km-g', 'km', 'Grande Comore', 'Island').
	subdivision('km-m', 'km', 'Mohéli', 'Island').
	subdivision('kn-01', 'kn', 'Christ Church Nichola Town', 'Parish').
	subdivision('kn-02', 'kn', 'Saint Anne Sandy Point', 'Parish').
	subdivision('kn-03', 'kn', 'Saint George Basseterre', 'Parish').
	subdivision('kn-04', 'kn', 'Saint George Gingerland', 'Parish').
	subdivision('kn-05', 'kn', 'Saint James Windward', 'Parish').
	subdivision('kn-06', 'kn', 'Saint John Capisterre', 'Parish').
	subdivision('kn-07', 'kn', 'Saint John Figtree', 'Parish').
	subdivision('kn-08', 'kn', 'Saint Mary Cayon', 'Parish').
	subdivision('kn-09', 'kn', 'Saint Paul Capisterre', 'Parish').
	subdivision('kn-10', 'kn', 'Saint Paul Charlestown', 'Parish').
	subdivision('kn-11', 'kn', 'Saint Peter Basseterre', 'Parish').
	subdivision('kn-12', 'kn', 'Saint Thomas Lowland', 'Parish').
	subdivision('kn-13', 'kn', 'Saint Thomas Middle Island', 'Parish').
	subdivision('kn-15', 'kn', 'Trinity Palmetto Point', 'Parish').
	subdivision('kn-k', 'kn', 'Saint Kitts', 'State').
	subdivision('kn-n', 'kn', 'Nevis', 'State').
	subdivision('kp-01', 'kp', 'Phyeongyang', 'Capital city').
	subdivision('kp-02', 'kp', 'Phyeongannamto', 'Province').
	subdivision('kp-03', 'kp', 'Phyeonganpukto', 'Province').
	subdivision('kp-04', 'kp', 'Jakangto', 'Province').
	subdivision('kp-05', 'kp', 'Hwanghainamto', 'Province').
	subdivision('kp-06', 'kp', 'Hwanghaipukto', 'Province').
	subdivision('kp-07', 'kp', 'Kangweonto', 'Province').
	subdivision('kp-08', 'kp', 'Hamkyeongnamto', 'Province').
	subdivision('kp-09', 'kp', 'Hamkyeongpukto', 'Province').
	subdivision('kp-10', 'kp', 'Ryangkangto', 'Province').
	subdivision('kp-13', 'kp', 'Raseon', 'Special city').
	subdivision('kp-14', 'kp', 'Nampho', 'Metropolitan city').
	subdivision('kp-15', 'kp', 'Kaeseong', 'Metropolitan city').
	subdivision('kr-11', 'kr', 'Seoul-teukbyeolsi', 'Special city').
	subdivision('kr-26', 'kr', 'Busan-gwangyeoksi', 'Metropolitan city').
	subdivision('kr-27', 'kr', 'Daegu-gwangyeoksi', 'Metropolitan city').
	subdivision('kr-28', 'kr', 'Incheon-gwangyeoksi', 'Metropolitan city').
	subdivision('kr-29', 'kr', 'Gwangju-gwangyeoksi', 'Metropolitan city').
	subdivision('kr-30', 'kr', 'Daejeon-gwangyeoksi', 'Metropolitan city').
	subdivision('kr-31', 'kr', 'Ulsan-gwangyeoksi', 'Metropolitan city').
	subdivision('kr-41', 'kr', 'Gyeonggi-do', 'Province').
	subdivision('kr-42', 'kr', 'Gangwon-teukbyeoljachido', 'Special self-governing province').
	subdivision('kr-43', 'kr', 'Chungcheongbuk-do', 'Province').
	subdivision('kr-44', 'kr', 'Chungcheongnam-do', 'Province').
	subdivision('kr-45', 'kr', 'Jeollabuk-do', 'Province').
	subdivision('kr-46', 'kr', 'Jeollanam-do', 'Province').
	subdivision('kr-47', 'kr', 'Gyeongsangbuk-do', 'Province').
	subdivision('kr-48', 'kr', 'Gyeongsangnam-do', 'Province').
	subdivision('kr-49', 'kr', 'Jeju-teukbyeoljachido', 'Special self-governing province').
	subdivision('kr-50', 'kr', 'Sejong', 'Special self-governing city').
	subdivision('kw-ah', 'kw', 'Al Aḩmadī', 'Governorate').
	subdivision('kw-fa', 'kw', 'Al Farwānīyah', 'Governorate').
	subdivision('kw-ha', 'kw', 'Ḩawallī', 'Governorate').
	subdivision('kw-ja', 'kw', 'Al Jahrā’', 'Governorate').
	subdivision('kw-ku', 'kw', 'Al ‘Āşimah', 'Governorate').
	subdivision('kw-mu', 'kw', 'Mubārak al Kabīr', 'Governorate').
	subdivision('kz-10', 'kz', 'Abay oblysy', 'Region').
	subdivision('kz-11', 'kz', 'Aqmola oblysy', 'Region').
	subdivision('kz-15', 'kz', 'Aqtöbe oblysy', 'Region').
	subdivision('kz-19', 'kz', 'Almaty oblysy', 'Region').
	subdivision('kz-23', 'kz', 'Atyraū oblysy', 'Region').
	subdivision('kz-27', 'kz', 'Batys Qazaqstan oblysy', 'Region').
	subdivision('kz-31', 'kz', 'Zhambyl oblysy', 'Region').
	subdivision('kz-33', 'kz', 'Zhetisū oblysy', 'Region').
	subdivision('kz-35', 'kz', 'Qaraghandy oblysy', 'Region').
	subdivision('kz-39', 'kz', 'Qostanay oblysy', 'Region').
	subdivision('kz-43', 'kz', 'Qyzylorda oblysy', 'Region').
	subdivision('kz-47', 'kz', 'Mangghystaū oblysy', 'Region').
	subdivision('kz-55', 'kz', 'Pavlodar oblysy', 'Region').
	subdivision('kz-59', 'kz', 'Soltüstik Qazaqstan oblysy', 'Region').
	subdivision('kz-61', 'kz', 'Türkistan oblysy', 'Region').
	subdivision('kz-62', 'kz', 'Ulytaū oblysy', 'Region').
	subdivision('kz-63', 'kz', 'Shyghys Qazaqstan oblysy', 'Region').
	subdivision('kz-71', 'kz', 'Astana', 'City').
	subdivision('kz-75', 'kz', 'Almaty', 'City').
	subdivision('kz-79', 'kz', 'Shymkent', 'City').
	subdivision('la-at', 'la', 'Attapu', 'Province').
	subdivision('la-bk', 'la', 'Bokèo', 'Province').
	subdivision('la-bl', 'la', 'Bolikhamxai', 'Province').
	subdivision('la-ch', 'la', 'Champasak', 'Province').
	subdivision('la-ho', 'la', 'Houaphan', 'Province').
	subdivision('la-kh', 'la', 'Khammouan', 'Province').
	subdivision('la-lm', 'la', 'Louang Namtha', 'Province').
	subdivision('la-lp', 'la', 'Louangphabang', 'Province').
	subdivision('la-ou', 'la', 'Oudômxai', 'Province').
	subdivision('la-ph', 'la', 'Phôngsali', 'Province').
	subdivision('la-sl', 'la', 'Salavan', 'Province').
	subdivision('la-sv', 'la', 'Savannakhét', 'Province').
	subdivision('la-vi', 'la', 'Viangchan', 'Province').
	subdivision('la-vt', 'la', 'Viangchan', 'Prefecture').
	subdivision('la-xa', 'la', 'Xaignabouli', 'Province').
	subdivision('la-xe', 'la', 'Xékong', 'Province').
	subdivision('la-xi', 'la', 'Xiangkhouang', 'Province').
	subdivision('la-xs', 'la', 'Xaisômboun', 'Province').
	subdivision('lb-ak', 'lb', '‘Akkār', 'Governorate').
	subdivision('lb-as', 'lb', 'Ash Shimāl', 'Governorate').
	subdivision('lb-ba', 'lb', 'Bayrūt', 'Governorate').
	subdivision('lb-bh', 'lb', 'B‘alabak-Al Hirmil', 'Governorate').
	subdivision('lb-bi', 'lb', 'Al Biqā‘', 'Governorate').
	subdivision('lb-ja', 'lb', 'Al Janūb', 'Governorate').
	subdivision('lb-jl', 'lb', 'Jabal Lubnān', 'Governorate').
	subdivision('lb-na', 'lb', 'An Nabaţīyah', 'Governorate').
	subdivision('lc-01', 'lc', 'Anse la Raye', 'District').
	subdivision('lc-02', 'lc', 'Castries', 'District').
	subdivision('lc-03', 'lc', 'Choiseul', 'District').
	subdivision('lc-05', 'lc', 'Dennery', 'District').
	subdivision('lc-06', 'lc', 'Gros Islet', 'District').
	subdivision('lc-07', 'lc', 'Laborie', 'District').
	subdivision('lc-08', 'lc', 'Micoud', 'District').
	subdivision('lc-10', 'lc', 'Soufrière', 'District').
	subdivision('lc-11', 'lc', 'Vieux Fort', 'District').
	subdivision('lc-12', 'lc', 'Canaries', 'District').
	subdivision('li-01', 'li', 'Balzers', 'Commune').
	subdivision('li-02', 'li', 'Eschen', 'Commune').
	subdivision('li-03', 'li', 'Gamprin', 'Commune').
	subdivision('li-04', 'li', 'Mauren', 'Commune').
	subdivision('li-05', 'li', 'Planken', 'Commune').
	subdivision('li-06', 'li', 'Ruggell', 'Commune').
	subdivision('li-07', 'li', 'Schaan', 'Commune').
	subdivision('li-08', 'li', 'Schellenberg', 'Commune').
	subdivision('li-09', 'li', 'Triesen', 'Commune').
	subdivision('li-10', 'li', 'Triesenberg', 'Commune').
	subdivision('li-11', 'li', 'Vaduz', 'Commune').
	subdivision('lk-1', 'lk', 'Western Province', 'Province').
	subdivision('lk-11', 'lk', 'Colombo', 'District').
	subdivision('lk-12', 'lk', 'Gampaha', 'District').
	subdivision('lk-13', 'lk', 'Kalutara', 'District').
	subdivision('lk-2', 'lk', 'Central Province', 'Province').
	subdivision('lk-21', 'lk', 'Kandy', 'District').
	subdivision('lk-22', 'lk', 'Matale', 'District').
	subdivision('lk-23', 'lk', 'Nuwara Eliya', 'District').
	subdivision('lk-3', 'lk', 'Southern Province', 'Province').
	subdivision('lk-31', 'lk', 'Galle', 'District').
	subdivision('lk-32', 'lk', 'Matara', 'District').
	subdivision('lk-33', 'lk', 'Hambantota', 'District').
	subdivision('lk-4', 'lk', 'Northern Province', 'Province').
	subdivision('lk-41', 'lk', 'Jaffna', 'District').
	subdivision('lk-42', 'lk', 'Kilinochchi', 'District').
	subdivision('lk-43', 'lk', 'Mannar', 'District').
	subdivision('lk-44', 'lk', 'Vavuniya', 'District').
	subdivision('lk-45', 'lk', 'Mullaittivu', 'District').
	subdivision('lk-5', 'lk', 'Eastern Province', 'Province').
	subdivision('lk-51', 'lk', 'Batticaloa', 'District').
	subdivision('lk-52', 'lk', 'Ampara', 'District').
	subdivision('lk-53', 'lk', 'Trincomalee', 'District').
	subdivision('lk-6', 'lk', 'North Western Province', 'Province').
	subdivision('lk-61', 'lk', 'Kurunegala', 'District').
	subdivision('lk-62', 'lk', 'Puttalam', 'District').
	subdivision('lk-7', 'lk', 'North Central Province', 'Province').
	subdivision('lk-71', 'lk', 'Anuradhapura', 'District').
	subdivision('lk-72', 'lk', 'Polonnaruwa', 'District').
	subdivision('lk-8', 'lk', 'Uva Province', 'Province').
	subdivision('lk-81', 'lk', 'Badulla', 'District').
	subdivision('lk-82', 'lk', 'Monaragala', 'District').
	subdivision('lk-9', 'lk', 'Sabaragamuwa Province', 'Province').
	subdivision('lk-91', 'lk', 'Ratnapura', 'District').
	subdivision('lk-92', 'lk', 'Kegalla', 'District').
	subdivision('lr-bg', 'lr', 'Bong', 'County').
	subdivision('lr-bm', 'lr', 'Bomi', 'County').
	subdivision('lr-cm', 'lr', 'Grand Cape Mount', 'County').
	subdivision('lr-gb', 'lr', 'Grand Bassa', 'County').
	subdivision('lr-gg', 'lr', 'Grand Gedeh', 'County').
	subdivision('lr-gk', 'lr', 'Grand Kru', 'County').
	subdivision('lr-gp', 'lr', 'Gbarpolu', 'County').
	subdivision('lr-lo', 'lr', 'Lofa', 'County').
	subdivision('lr-mg', 'lr', 'Margibi', 'County').
	subdivision('lr-mo', 'lr', 'Montserrado', 'County').
	subdivision('lr-my', 'lr', 'Maryland', 'County').
	subdivision('lr-ni', 'lr', 'Nimba', 'County').
	subdivision('lr-rg', 'lr', 'River Gee', 'County').
	subdivision('lr-ri', 'lr', 'River Cess', 'County').
	subdivision('lr-si', 'lr', 'Sinoe', 'County').
	subdivision('ls-a', 'ls', 'Maseru', 'District').
	subdivision('ls-b', 'ls', 'Botha-Bothe', 'District').
	subdivision('ls-c', 'ls', 'Leribe', 'District').
	subdivision('ls-d', 'ls', 'Berea', 'District').
	subdivision('ls-e', 'ls', 'Mafeteng', 'District').
	subdivision('ls-f', 'ls', 'Mohale''s Hoek', 'District').
	subdivision('ls-g', 'ls', 'Quthing', 'District').
	subdivision('ls-h', 'ls', 'Qacha''s Nek', 'District').
	subdivision('ls-j', 'ls', 'Mokhotlong', 'District').
	subdivision('ls-k', 'ls', 'Thaba-Tseka', 'District').
	subdivision('lt-01', 'lt', 'Akmenė', 'District municipality').
	subdivision('lt-02', 'lt', 'Alytaus miestas', 'City municipality').
	subdivision('lt-03', 'lt', 'Alytus', 'District municipality').
	subdivision('lt-04', 'lt', 'Anykščiai', 'District municipality').
	subdivision('lt-05', 'lt', 'Birštonas', 'Municipality').
	subdivision('lt-06', 'lt', 'Biržai', 'District municipality').
	subdivision('lt-07', 'lt', 'Druskininkai', 'Municipality').
	subdivision('lt-08', 'lt', 'Elektrėnai', 'Municipality').
	subdivision('lt-09', 'lt', 'Ignalina', 'District municipality').
	subdivision('lt-10', 'lt', 'Jonava', 'District municipality').
	subdivision('lt-11', 'lt', 'Joniškis', 'District municipality').
	subdivision('lt-12', 'lt', 'Jurbarkas', 'District municipality').
	subdivision('lt-13', 'lt', 'Kaišiadorys', 'District municipality').
	subdivision('lt-14', 'lt', 'Kalvarija', 'Municipality').
	subdivision('lt-15', 'lt', 'Kauno miestas', 'City municipality').
	subdivision('lt-16', 'lt', 'Kaunas', 'District municipality').
	subdivision('lt-17', 'lt', 'Kazlų Rūdos', 'Municipality').
	subdivision('lt-18', 'lt', 'Kėdainiai', 'District municipality').
	subdivision('lt-19', 'lt', 'Kelmė', 'District municipality').
	subdivision('lt-20', 'lt', 'Klaipėdos miestas', 'City municipality').
	subdivision('lt-21', 'lt', 'Klaipėda', 'District municipality').
	subdivision('lt-22', 'lt', 'Kretinga', 'District municipality').
	subdivision('lt-23', 'lt', 'Kupiškis', 'District municipality').
	subdivision('lt-24', 'lt', 'Lazdijai', 'District municipality').
	subdivision('lt-25', 'lt', 'Marijampolė', 'District municipality').
	subdivision('lt-26', 'lt', 'Mažeikiai', 'District municipality').
	subdivision('lt-27', 'lt', 'Molėtai', 'District municipality').
	subdivision('lt-28', 'lt', 'Neringa', 'Municipality').
	subdivision('lt-29', 'lt', 'Pagėgiai', 'Municipality').
	subdivision('lt-30', 'lt', 'Pakruojis', 'District municipality').
	subdivision('lt-31', 'lt', 'Palangos miestas', 'City municipality').
	subdivision('lt-32', 'lt', 'Panevėžio miestas', 'City municipality').
	subdivision('lt-33', 'lt', 'Panevėžys', 'District municipality').
	subdivision('lt-34', 'lt', 'Pasvalys', 'District municipality').
	subdivision('lt-35', 'lt', 'Plungė', 'District municipality').
	subdivision('lt-36', 'lt', 'Prienai', 'District municipality').
	subdivision('lt-37', 'lt', 'Radviliškis', 'District municipality').
	subdivision('lt-38', 'lt', 'Raseiniai', 'District municipality').
	subdivision('lt-39', 'lt', 'Rietavas', 'Municipality').
	subdivision('lt-40', 'lt', 'Rokiškis', 'District municipality').
	subdivision('lt-41', 'lt', 'Šakiai', 'District municipality').
	subdivision('lt-42', 'lt', 'Šalčininkai', 'District municipality').
	subdivision('lt-43', 'lt', 'Šiaulių miestas', 'City municipality').
	subdivision('lt-44', 'lt', 'Šiauliai', 'District municipality').
	subdivision('lt-45', 'lt', 'Šilalė', 'District municipality').
	subdivision('lt-46', 'lt', 'Šilutė', 'District municipality').
	subdivision('lt-47', 'lt', 'Širvintos', 'District municipality').
	subdivision('lt-48', 'lt', 'Skuodas', 'District municipality').
	subdivision('lt-49', 'lt', 'Švenčionys', 'District municipality').
	subdivision('lt-50', 'lt', 'Tauragė', 'District municipality').
	subdivision('lt-51', 'lt', 'Telšiai', 'District municipality').
	subdivision('lt-52', 'lt', 'Trakai', 'District municipality').
	subdivision('lt-53', 'lt', 'Ukmergė', 'District municipality').
	subdivision('lt-54', 'lt', 'Utena', 'District municipality').
	subdivision('lt-55', 'lt', 'Varėna', 'District municipality').
	subdivision('lt-56', 'lt', 'Vilkaviškis', 'District municipality').
	subdivision('lt-57', 'lt', 'Vilniaus miestas', 'City municipality').
	subdivision('lt-58', 'lt', 'Vilnius', 'District municipality').
	subdivision('lt-59', 'lt', 'Visaginas', 'Municipality').
	subdivision('lt-60', 'lt', 'Zarasai', 'District municipality').
	subdivision('lt-al', 'lt', 'Alytaus apskritis', 'County').
	subdivision('lt-kl', 'lt', 'Klaipėdos apskritis', 'County').
	subdivision('lt-ku', 'lt', 'Kauno apskritis', 'County').
	subdivision('lt-mr', 'lt', 'Marijampolės apskritis', 'County').
	subdivision('lt-pn', 'lt', 'Panevėžio apskritis', 'County').
	subdivision('lt-sa', 'lt', 'Šiaulių apskritis', 'County').
	subdivision('lt-ta', 'lt', 'Tauragės apskritis', 'County').
	subdivision('lt-te', 'lt', 'Telšių apskritis', 'County').
	subdivision('lt-ut', 'lt', 'Utenos apskritis', 'County').
	subdivision('lt-vl', 'lt', 'Vilniaus apskritis', 'County').
	subdivision('lu-ca', 'lu', 'Capellen', 'Canton').
	subdivision('lu-cl', 'lu', 'Clervaux', 'Canton').
	subdivision('lu-di', 'lu', 'Diekirch', 'Canton').
	subdivision('lu-ec', 'lu', 'Echternach', 'Canton').
	subdivision('lu-es', 'lu', 'Esch-sur-Alzette', 'Canton').
	subdivision('lu-gr', 'lu', 'Grevenmacher', 'Canton').
	subdivision('lu-lu', 'lu', 'Luxembourg', 'Canton').
	subdivision('lu-me', 'lu', 'Mersch', 'Canton').
	subdivision('lu-rd', 'lu', 'Redange', 'Canton').
	subdivision('lu-rm', 'lu', 'Remich', 'Canton').
	subdivision('lu-vd', 'lu', 'Vianden', 'Canton').
	subdivision('lu-wi', 'lu', 'Wiltz', 'Canton').
	subdivision('lv-002', 'lv', 'Aizkraukles novads', 'Municipality').
	subdivision('lv-007', 'lv', 'Alūksnes novads', 'Municipality').
	subdivision('lv-011', 'lv', 'Ādažu novads', 'Municipality').
	subdivision('lv-015', 'lv', 'Balvu novads', 'Municipality').
	subdivision('lv-016', 'lv', 'Bauskas novads', 'Municipality').
	subdivision('lv-022', 'lv', 'Cēsu novads', 'Municipality').
	subdivision('lv-026', 'lv', 'Dobeles novads', 'Municipality').
	subdivision('lv-033', 'lv', 'Gulbenes novads', 'Municipality').
	subdivision('lv-041', 'lv', 'Jelgavas novads', 'Municipality').
	subdivision('lv-042', 'lv', 'Jēkabpils novads', 'Municipality').
	subdivision('lv-047', 'lv', 'Krāslavas novads', 'Municipality').
	subdivision('lv-050', 'lv', 'Kuldīgas novads', 'Municipality').
	subdivision('lv-052', 'lv', 'Ķekavas novads', 'Municipality').
	subdivision('lv-054', 'lv', 'Limbažu novads', 'Municipality').
	subdivision('lv-056', 'lv', 'Līvānu novads', 'Municipality').
	subdivision('lv-058', 'lv', 'Ludzas novads', 'Municipality').
	subdivision('lv-059', 'lv', 'Madonas novads', 'Municipality').
	subdivision('lv-062', 'lv', 'Mārupes novads', 'Municipality').
	subdivision('lv-067', 'lv', 'Ogres novads', 'Municipality').
	subdivision('lv-068', 'lv', 'Olaines novads', 'Municipality').
	subdivision('lv-073', 'lv', 'Preiļu novads', 'Municipality').
	subdivision('lv-077', 'lv', 'Rēzeknes novads', 'Municipality').
	subdivision('lv-080', 'lv', 'Ropažu novads', 'Municipality').
	subdivision('lv-087', 'lv', 'Salaspils novads', 'Municipality').
	subdivision('lv-088', 'lv', 'Saldus novads', 'Municipality').
	subdivision('lv-089', 'lv', 'Saulkrastu novads', 'Municipality').
	subdivision('lv-091', 'lv', 'Siguldas novads', 'Municipality').
	subdivision('lv-094', 'lv', 'Smiltenes novads', 'Municipality').
	subdivision('lv-097', 'lv', 'Talsu novads', 'Municipality').
	subdivision('lv-099', 'lv', 'Tukuma novads', 'Municipality').
	subdivision('lv-101', 'lv', 'Valkas novads', 'Municipality').
	subdivision('lv-102', 'lv', 'Varakļānu novads', 'Municipality').
	subdivision('lv-106', 'lv', 'Ventspils novads', 'Municipality').
	subdivision('lv-111', 'lv', 'Augšdaugavas novads', 'Municipality').
	subdivision('lv-112', 'lv', 'Dienvidkurzemes Novads', 'Municipality').
	subdivision('lv-113', 'lv', 'Valmieras Novads', 'Municipality').
	subdivision('lv-dgv', 'lv', 'Daugavpils', 'State city').
	subdivision('lv-jel', 'lv', 'Jelgava', 'State city').
	subdivision('lv-jur', 'lv', 'Jūrmala', 'State city').
	subdivision('lv-lpx', 'lv', 'Liepāja', 'State city').
	subdivision('lv-rez', 'lv', 'Rēzekne', 'State city').
	subdivision('lv-rix', 'lv', 'Rīga', 'State city').
	subdivision('lv-ven', 'lv', 'Ventspils', 'State city').
	subdivision('ly-ba', 'ly', 'Banghāzī', 'Popularate').
	subdivision('ly-bu', 'ly', 'Al Buţnān', 'Popularate').
	subdivision('ly-dr', 'ly', 'Darnah', 'Popularate').
	subdivision('ly-gt', 'ly', 'Ghāt', 'Popularate').
	subdivision('ly-ja', 'ly', 'Al Jabal al Akhḑar', 'Popularate').
	subdivision('ly-jg', 'ly', 'Al Jabal al Gharbī', 'Popularate').
	subdivision('ly-ji', 'ly', 'Al Jafārah', 'Popularate').
	subdivision('ly-ju', 'ly', 'Al Jufrah', 'Popularate').
	subdivision('ly-kf', 'ly', 'Al Kufrah', 'Popularate').
	subdivision('ly-mb', 'ly', 'Al Marqab', 'Popularate').
	subdivision('ly-mi', 'ly', 'Mişrātah', 'Popularate').
	subdivision('ly-mj', 'ly', 'Al Marj', 'Popularate').
	subdivision('ly-mq', 'ly', 'Murzuq', 'Popularate').
	subdivision('ly-nl', 'ly', 'Nālūt', 'Popularate').
	subdivision('ly-nq', 'ly', 'An Nuqāţ al Khams', 'Popularate').
	subdivision('ly-sb', 'ly', 'Sabhā', 'Popularate').
	subdivision('ly-sr', 'ly', 'Surt', 'Popularate').
	subdivision('ly-tb', 'ly', 'Ţarābulus', 'Popularate').
	subdivision('ly-wa', 'ly', 'Al Wāḩāt', 'Popularate').
	subdivision('ly-wd', 'ly', 'Wādī al Ḩayāt', 'Popularate').
	subdivision('ly-ws', 'ly', 'Wādī ash Shāţi’', 'Popularate').
	subdivision('ly-za', 'ly', 'Az Zāwiyah', 'Popularate').
	subdivision('ma-01', 'ma', 'Tanger-Tétouan-Al Hoceïma', 'Region').
	subdivision('ma-02', 'ma', 'L''Oriental', 'Region').
	subdivision('ma-03', 'ma', 'Fès-Meknès', 'Region').
	subdivision('ma-04', 'ma', 'Rabat-Salé-Kénitra', 'Region').
	subdivision('ma-05', 'ma', 'Béni Mellal-Khénifra', 'Region').
	subdivision('ma-06', 'ma', 'Casablanca-Settat', 'Region').
	subdivision('ma-07', 'ma', 'Marrakech-Safi', 'Region').
	subdivision('ma-08', 'ma', 'Drâa-Tafilalet', 'Region').
	subdivision('ma-09', 'ma', 'Souss-Massa', 'Region').
	subdivision('ma-10', 'ma', 'Guelmim-Oued Noun (EH-partial)', 'Region').
	subdivision('ma-11', 'ma', 'Laâyoune-Sakia El Hamra (EH-partial)', 'Region').
	subdivision('ma-12', 'ma', 'Dakhla-Oued Ed-Dahab (EH)', 'Region').
	subdivision('ma-agd', 'ma', 'Agadir-Ida-Ou-Tanane', 'Prefecture').
	subdivision('ma-aou', 'ma', 'Aousserd (EH)', 'Province').
	subdivision('ma-asz', 'ma', 'Assa-Zag (EH-partial)', 'Province').
	subdivision('ma-azi', 'ma', 'Azilal', 'Province').
	subdivision('ma-bem', 'ma', 'Béni Mellal', 'Province').
	subdivision('ma-ber', 'ma', 'Berkane', 'Province').
	subdivision('ma-bes', 'ma', 'Benslimane', 'Province').
	subdivision('ma-bod', 'ma', 'Boujdour (EH)', 'Province').
	subdivision('ma-bom', 'ma', 'Boulemane', 'Province').
	subdivision('ma-brr', 'ma', 'Berrechid', 'Province').
	subdivision('ma-cas', 'ma', 'Casablanca', 'Prefecture').
	subdivision('ma-che', 'ma', 'Chefchaouen', 'Province').
	subdivision('ma-chi', 'ma', 'Chichaoua', 'Province').
	subdivision('ma-cht', 'ma', 'Chtouka-Ait Baha', 'Province').
	subdivision('ma-dri', 'ma', 'Driouch', 'Province').
	subdivision('ma-err', 'ma', 'Errachidia', 'Province').
	subdivision('ma-esi', 'ma', 'Essaouira', 'Province').
	subdivision('ma-esm', 'ma', 'Es-Semara (EH-partial)', 'Province').
	subdivision('ma-fah', 'ma', 'Fahs-Anjra', 'Province').
	subdivision('ma-fes', 'ma', 'Fès', 'Prefecture').
	subdivision('ma-fig', 'ma', 'Figuig', 'Province').
	subdivision('ma-fqh', 'ma', 'Fquih Ben Salah', 'Province').
	subdivision('ma-gue', 'ma', 'Guelmim', 'Province').
	subdivision('ma-guf', 'ma', 'Guercif', 'Province').
	subdivision('ma-haj', 'ma', 'El Hajeb', 'Province').
	subdivision('ma-hao', 'ma', 'Al Haouz', 'Province').
	subdivision('ma-hoc', 'ma', 'Al Hoceïma', 'Province').
	subdivision('ma-ifr', 'ma', 'Ifrane', 'Province').
	subdivision('ma-ine', 'ma', 'Inezgane-Ait Melloul', 'Prefecture').
	subdivision('ma-jdi', 'ma', 'El Jadida', 'Province').
	subdivision('ma-jra', 'ma', 'Jerada', 'Province').
	subdivision('ma-ken', 'ma', 'Kénitra', 'Province').
	subdivision('ma-kes', 'ma', 'El Kelâa des Sraghna', 'Province').
	subdivision('ma-khe', 'ma', 'Khémisset', 'Province').
	subdivision('ma-khn', 'ma', 'Khénifra', 'Province').
	subdivision('ma-kho', 'ma', 'Khouribga', 'Province').
	subdivision('ma-laa', 'ma', 'Laâyoune (EH)', 'Province').
	subdivision('ma-lar', 'ma', 'Larache', 'Province').
	subdivision('ma-mar', 'ma', 'Marrakech', 'Prefecture').
	subdivision('ma-mdf', 'ma', 'M’diq-Fnideq', 'Prefecture').
	subdivision('ma-med', 'ma', 'Médiouna', 'Province').
	subdivision('ma-mek', 'ma', 'Meknès', 'Prefecture').
	subdivision('ma-mid', 'ma', 'Midelt', 'Province').
	subdivision('ma-moh', 'ma', 'Mohammadia', 'Prefecture').
	subdivision('ma-mou', 'ma', 'Moulay Yacoub', 'Province').
	subdivision('ma-nad', 'ma', 'Nador', 'Province').
	subdivision('ma-nou', 'ma', 'Nouaceur', 'Province').
	subdivision('ma-oua', 'ma', 'Ouarzazate', 'Province').
	subdivision('ma-oud', 'ma', 'Oued Ed-Dahab (EH)', 'Province').
	subdivision('ma-ouj', 'ma', 'Oujda-Angad', 'Prefecture').
	subdivision('ma-ouz', 'ma', 'Ouezzane', 'Province').
	subdivision('ma-rab', 'ma', 'Rabat', 'Prefecture').
	subdivision('ma-reh', 'ma', 'Rehamna', 'Province').
	subdivision('ma-saf', 'ma', 'Safi', 'Province').
	subdivision('ma-sal', 'ma', 'Salé', 'Prefecture').
	subdivision('ma-sef', 'ma', 'Sefrou', 'Province').
	subdivision('ma-set', 'ma', 'Settat', 'Province').
	subdivision('ma-sib', 'ma', 'Sidi Bennour', 'Province').
	subdivision('ma-sif', 'ma', 'Sidi Ifni', 'Province').
	subdivision('ma-sik', 'ma', 'Sidi Kacem', 'Province').
	subdivision('ma-sil', 'ma', 'Sidi Slimane', 'Province').
	subdivision('ma-skh', 'ma', 'Skhirate-Témara', 'Prefecture').
	subdivision('ma-taf', 'ma', 'Tarfaya (EH-partial)', 'Province').
	subdivision('ma-tai', 'ma', 'Taourirt', 'Province').
	subdivision('ma-tao', 'ma', 'Taounate', 'Province').
	subdivision('ma-tar', 'ma', 'Taroudannt', 'Province').
	subdivision('ma-tat', 'ma', 'Tata', 'Province').
	subdivision('ma-taz', 'ma', 'Taza', 'Province').
	subdivision('ma-tet', 'ma', 'Tétouan', 'Province').
	subdivision('ma-tin', 'ma', 'Tinghir', 'Province').
	subdivision('ma-tiz', 'ma', 'Tiznit', 'Province').
	subdivision('ma-tng', 'ma', 'Tanger-Assilah', 'Prefecture').
	subdivision('ma-tnt', 'ma', 'Tan-Tan (EH-partial)', 'Province').
	subdivision('ma-yus', 'ma', 'Youssoufia', 'Province').
	subdivision('ma-zag', 'ma', 'Zagora', 'Province').
	subdivision('mc-cl', 'mc', 'La Colle', 'Quarter').
	subdivision('mc-co', 'mc', 'La Condamine', 'Quarter').
	subdivision('mc-fo', 'mc', 'Fontvieille', 'Quarter').
	subdivision('mc-ga', 'mc', 'La Gare', 'Quarter').
	subdivision('mc-je', 'mc', 'Jardin Exotique', 'Quarter').
	subdivision('mc-la', 'mc', 'Larvotto', 'Quarter').
	subdivision('mc-ma', 'mc', 'Malbousquet', 'Quarter').
	subdivision('mc-mc', 'mc', 'Monte-Carlo', 'Quarter').
	subdivision('mc-mg', 'mc', 'Moneghetti', 'Quarter').
	subdivision('mc-mo', 'mc', 'Monaco-Ville', 'Quarter').
	subdivision('mc-mu', 'mc', 'Moulins', 'Quarter').
	subdivision('mc-ph', 'mc', 'Port-Hercule', 'Quarter').
	subdivision('mc-sd', 'mc', 'Sainte-Dévote', 'Quarter').
	subdivision('mc-so', 'mc', 'La Source', 'Quarter').
	subdivision('mc-sp', 'mc', 'Spélugues', 'Quarter').
	subdivision('mc-sr', 'mc', 'Saint-Roman', 'Quarter').
	subdivision('mc-vr', 'mc', 'Vallon de la Rousse', 'Quarter').
	subdivision('md-an', 'md', 'Anenii Noi', 'District').
	subdivision('md-ba', 'md', 'Bălți', 'City').
	subdivision('md-bd', 'md', 'Bender [Tighina]', 'City').
	subdivision('md-br', 'md', 'Briceni', 'District').
	subdivision('md-bs', 'md', 'Basarabeasca', 'District').
	subdivision('md-ca', 'md', 'Cahul', 'District').
	subdivision('md-cl', 'md', 'Călărași', 'District').
	subdivision('md-cm', 'md', 'Cimișlia', 'District').
	subdivision('md-cr', 'md', 'Criuleni', 'District').
	subdivision('md-cs', 'md', 'Căușeni', 'District').
	subdivision('md-ct', 'md', 'Cantemir', 'District').
	subdivision('md-cu', 'md', 'Chișinău', 'City').
	subdivision('md-do', 'md', 'Dondușeni', 'District').
	subdivision('md-dr', 'md', 'Drochia', 'District').
	subdivision('md-du', 'md', 'Dubăsari', 'District').
	subdivision('md-ed', 'md', 'Edineț', 'District').
	subdivision('md-fa', 'md', 'Fălești', 'District').
	subdivision('md-fl', 'md', 'Florești', 'District').
	subdivision('md-ga', 'md', 'Găgăuzia, Unitatea teritorială autonomă (UTAG)', 'Autonomous territorial unit').
	subdivision('md-gl', 'md', 'Glodeni', 'District').
	subdivision('md-hi', 'md', 'Hîncești', 'District').
	subdivision('md-ia', 'md', 'Ialoveni', 'District').
	subdivision('md-le', 'md', 'Leova', 'District').
	subdivision('md-ni', 'md', 'Nisporeni', 'District').
	subdivision('md-oc', 'md', 'Ocnița', 'District').
	subdivision('md-or', 'md', 'Orhei', 'District').
	subdivision('md-re', 'md', 'Rezina', 'District').
	subdivision('md-ri', 'md', 'Rîșcani', 'District').
	subdivision('md-sd', 'md', 'Șoldănești', 'District').
	subdivision('md-si', 'md', 'Sîngerei', 'District').
	subdivision('md-sn', 'md', 'Stînga Nistrului, unitatea teritorială din', 'Territorial unit').
	subdivision('md-so', 'md', 'Soroca', 'District').
	subdivision('md-st', 'md', 'Strășeni', 'District').
	subdivision('md-sv', 'md', 'Ștefan Vodă', 'District').
	subdivision('md-ta', 'md', 'Taraclia', 'District').
	subdivision('md-te', 'md', 'Telenești', 'District').
	subdivision('md-un', 'md', 'Ungheni', 'District').
	subdivision('me-01', 'me', 'Andrijevica', 'Municipality').
	subdivision('me-02', 'me', 'Bar', 'Municipality').
	subdivision('me-03', 'me', 'Berane', 'Municipality').
	subdivision('me-04', 'me', 'Bijelo Polje', 'Municipality').
	subdivision('me-05', 'me', 'Budva', 'Municipality').
	subdivision('me-06', 'me', 'Cetinje', 'Municipality').
	subdivision('me-07', 'me', 'Danilovgrad', 'Municipality').
	subdivision('me-08', 'me', 'Herceg-Novi', 'Municipality').
	subdivision('me-09', 'me', 'Kolašin', 'Municipality').
	subdivision('me-10', 'me', 'Kotor', 'Municipality').
	subdivision('me-11', 'me', 'Mojkovac', 'Municipality').
	subdivision('me-12', 'me', 'Nikšić', 'Municipality').
	subdivision('me-13', 'me', 'Plav', 'Municipality').
	subdivision('me-14', 'me', 'Pljevlja', 'Municipality').
	subdivision('me-15', 'me', 'Plužine', 'Municipality').
	subdivision('me-16', 'me', 'Podgorica', 'Municipality').
	subdivision('me-17', 'me', 'Rožaje', 'Municipality').
	subdivision('me-18', 'me', 'Šavnik', 'Municipality').
	subdivision('me-19', 'me', 'Tivat', 'Municipality').
	subdivision('me-20', 'me', 'Ulcinj', 'Municipality').
	subdivision('me-21', 'me', 'Žabljak', 'Municipality').
	subdivision('me-22', 'me', 'Gusinje', 'Municipality').
	subdivision('me-23', 'me', 'Petnjica', 'Municipality').
	subdivision('me-24', 'me', 'Tuzi', 'Municipality').
	subdivision('me-25', 'me', 'Zeta', 'Municipality').
	subdivision('mg-a', 'mg', 'Toamasina', 'Province').
	subdivision('mg-d', 'mg', 'Antsiranana', 'Province').
	subdivision('mg-f', 'mg', 'Fianarantsoa', 'Province').
	subdivision('mg-m', 'mg', 'Mahajanga', 'Province').
	subdivision('mg-t', 'mg', 'Antananarivo', 'Province').
	subdivision('mg-u', 'mg', 'Toliara', 'Province').
	subdivision('mh-alk', 'mh', 'Ailuk', 'Municipality').
	subdivision('mh-all', 'mh', 'Ailinglaplap', 'Municipality').
	subdivision('mh-arn', 'mh', 'Arno', 'Municipality').
	subdivision('mh-aur', 'mh', 'Aur', 'Municipality').
	subdivision('mh-ebo', 'mh', 'Ebon', 'Municipality').
	subdivision('mh-eni', 'mh', 'Enewetak & Ujelang', 'Municipality').
	subdivision('mh-jab', 'mh', 'Jabat', 'Municipality').
	subdivision('mh-jal', 'mh', 'Jaluit', 'Municipality').
	subdivision('mh-kil', 'mh', 'Bikini & Kili', 'Municipality').
	subdivision('mh-kwa', 'mh', 'Kwajalein', 'Municipality').
	subdivision('mh-l', 'mh', 'Ralik chain', 'Chain (of islands)').
	subdivision('mh-lae', 'mh', 'Lae', 'Municipality').
	subdivision('mh-lib', 'mh', 'Lib', 'Municipality').
	subdivision('mh-lik', 'mh', 'Likiep', 'Municipality').
	subdivision('mh-maj', 'mh', 'Majuro', 'Municipality').
	subdivision('mh-mal', 'mh', 'Maloelap', 'Municipality').
	subdivision('mh-mej', 'mh', 'Mejit', 'Municipality').
	subdivision('mh-mil', 'mh', 'Mili', 'Municipality').
	subdivision('mh-nmk', 'mh', 'Namdrik', 'Municipality').
	subdivision('mh-nmu', 'mh', 'Namu', 'Municipality').
	subdivision('mh-ron', 'mh', 'Rongelap', 'Municipality').
	subdivision('mh-t', 'mh', 'Ratak chain', 'Chain (of islands)').
	subdivision('mh-uja', 'mh', 'Ujae', 'Municipality').
	subdivision('mh-uti', 'mh', 'Utrik', 'Municipality').
	subdivision('mh-wth', 'mh', 'Wotho', 'Municipality').
	subdivision('mh-wtj', 'mh', 'Wotje', 'Municipality').
	subdivision('mk-101', 'mk', 'Veles', 'Municipality').
	subdivision('mk-102', 'mk', 'Gradsko', 'Municipality').
	subdivision('mk-103', 'mk', 'Demir Kapija', 'Municipality').
	subdivision('mk-104', 'mk', 'Kavadarci', 'Municipality').
	subdivision('mk-105', 'mk', 'Lozovo', 'Municipality').
	subdivision('mk-106', 'mk', 'Negotino', 'Municipality').
	subdivision('mk-107', 'mk', 'Rosoman', 'Municipality').
	subdivision('mk-108', 'mk', 'Sveti Nikole', 'Municipality').
	subdivision('mk-109', 'mk', 'Čaška', 'Municipality').
	subdivision('mk-201', 'mk', 'Berovo', 'Municipality').
	subdivision('mk-202', 'mk', 'Vinica', 'Municipality').
	subdivision('mk-203', 'mk', 'Delčevo', 'Municipality').
	subdivision('mk-204', 'mk', 'Zrnovci', 'Municipality').
	subdivision('mk-205', 'mk', 'Karbinci', 'Municipality').
	subdivision('mk-206', 'mk', 'Kočani', 'Municipality').
	subdivision('mk-207', 'mk', 'Makedonska Kamenica', 'Municipality').
	subdivision('mk-208', 'mk', 'Pehčevo', 'Municipality').
	subdivision('mk-209', 'mk', 'Probištip', 'Municipality').
	subdivision('mk-210', 'mk', 'Češinovo-Obleševo', 'Municipality').
	subdivision('mk-211', 'mk', 'Štip', 'Municipality').
	subdivision('mk-301', 'mk', 'Vevčani', 'Municipality').
	subdivision('mk-303', 'mk', 'Debar', 'Municipality').
	subdivision('mk-304', 'mk', 'Debrca', 'Municipality').
	subdivision('mk-307', 'mk', 'Kičevo', 'Municipality').
	subdivision('mk-308', 'mk', 'Makedonski Brod', 'Municipality').
	subdivision('mk-310', 'mk', 'Ohrid', 'Municipality').
	subdivision('mk-311', 'mk', 'Plasnica', 'Municipality').
	subdivision('mk-312', 'mk', 'Struga', 'Municipality').
	subdivision('mk-313', 'mk', 'Centar Župa', 'Municipality').
	subdivision('mk-401', 'mk', 'Bogdanci', 'Municipality').
	subdivision('mk-402', 'mk', 'Bosilovo', 'Municipality').
	subdivision('mk-403', 'mk', 'Valandovo', 'Municipality').
	subdivision('mk-404', 'mk', 'Vasilevo', 'Municipality').
	subdivision('mk-405', 'mk', 'Gevgelija', 'Municipality').
	subdivision('mk-406', 'mk', 'Dojran', 'Municipality').
	subdivision('mk-407', 'mk', 'Konče', 'Municipality').
	subdivision('mk-408', 'mk', 'Novo Selo', 'Municipality').
	subdivision('mk-409', 'mk', 'Radoviš', 'Municipality').
	subdivision('mk-410', 'mk', 'Strumica', 'Municipality').
	subdivision('mk-501', 'mk', 'Bitola', 'Municipality').
	subdivision('mk-502', 'mk', 'Demir Hisar', 'Municipality').
	subdivision('mk-503', 'mk', 'Dolneni', 'Municipality').
	subdivision('mk-504', 'mk', 'Krivogaštani', 'Municipality').
	subdivision('mk-505', 'mk', 'Kruševo', 'Municipality').
	subdivision('mk-506', 'mk', 'Mogila', 'Municipality').
	subdivision('mk-507', 'mk', 'Novaci', 'Municipality').
	subdivision('mk-508', 'mk', 'Prilep', 'Municipality').
	subdivision('mk-509', 'mk', 'Resen', 'Municipality').
	subdivision('mk-601', 'mk', 'Bogovinje', 'Municipality').
	subdivision('mk-602', 'mk', 'Brvenica', 'Municipality').
	subdivision('mk-603', 'mk', 'Vrapčište', 'Municipality').
	subdivision('mk-604', 'mk', 'Gostivar', 'Municipality').
	subdivision('mk-605', 'mk', 'Želino', 'Municipality').
	subdivision('mk-606', 'mk', 'Jegunovce', 'Municipality').
	subdivision('mk-607', 'mk', 'Mavrovo i Rostuše', 'Municipality').
	subdivision('mk-608', 'mk', 'Tearce', 'Municipality').
	subdivision('mk-609', 'mk', 'Tetovo', 'Municipality').
	subdivision('mk-701', 'mk', 'Kratovo', 'Municipality').
	subdivision('mk-702', 'mk', 'Kriva Palanka', 'Municipality').
	subdivision('mk-703', 'mk', 'Kumanovo', 'Municipality').
	subdivision('mk-704', 'mk', 'Lipkovo', 'Municipality').
	subdivision('mk-705', 'mk', 'Rankovce', 'Municipality').
	subdivision('mk-706', 'mk', 'Staro Nagoričane', 'Municipality').
	subdivision('mk-801', 'mk', 'Aerodrom †', 'Municipality').
	subdivision('mk-802', 'mk', 'Aračinovo', 'Municipality').
	subdivision('mk-803', 'mk', 'Butel †', 'Municipality').
	subdivision('mk-804', 'mk', 'Gazi Baba †', 'Municipality').
	subdivision('mk-805', 'mk', 'Gjorče Petrov †', 'Municipality').
	subdivision('mk-806', 'mk', 'Zelenikovo', 'Municipality').
	subdivision('mk-807', 'mk', 'Ilinden', 'Municipality').
	subdivision('mk-808', 'mk', 'Karpoš †', 'Municipality').
	subdivision('mk-809', 'mk', 'Kisela Voda †', 'Municipality').
	subdivision('mk-810', 'mk', 'Petrovec', 'Municipality').
	subdivision('mk-811', 'mk', 'Saraj †', 'Municipality').
	subdivision('mk-812', 'mk', 'Sopište', 'Municipality').
	subdivision('mk-813', 'mk', 'Studeničani', 'Municipality').
	subdivision('mk-814', 'mk', 'Centar †', 'Municipality').
	subdivision('mk-815', 'mk', 'Čair †', 'Municipality').
	subdivision('mk-816', 'mk', 'Čučer-Sandevo', 'Municipality').
	subdivision('mk-817', 'mk', 'Šuto Orizari †', 'Municipality').
	subdivision('ml-1', 'ml', 'Kayes', 'Region').
	subdivision('ml-10', 'ml', 'Taoudénit', 'Region').
	subdivision('ml-2', 'ml', 'Koulikoro', 'Region').
	subdivision('ml-3', 'ml', 'Sikasso', 'Region').
	subdivision('ml-4', 'ml', 'Ségou', 'Region').
	subdivision('ml-5', 'ml', 'Mopti', 'Region').
	subdivision('ml-6', 'ml', 'Tombouctou', 'Region').
	subdivision('ml-7', 'ml', 'Gao', 'Region').
	subdivision('ml-8', 'ml', 'Kidal', 'Region').
	subdivision('ml-9', 'ml', 'Ménaka', 'Region').
	subdivision('ml-bko', 'ml', 'Bamako', 'District').
	subdivision('mm-01', 'mm', 'Sagaing', 'Region').
	subdivision('mm-02', 'mm', 'Bago', 'Region').
	subdivision('mm-03', 'mm', 'Magway', 'Region').
	subdivision('mm-04', 'mm', 'Mandalay', 'Region').
	subdivision('mm-05', 'mm', 'Tanintharyi', 'Region').
	subdivision('mm-06', 'mm', 'Yangon', 'Region').
	subdivision('mm-07', 'mm', 'Ayeyarwady', 'Region').
	subdivision('mm-11', 'mm', 'Kachin', 'State').
	subdivision('mm-12', 'mm', 'Kayah', 'State').
	subdivision('mm-13', 'mm', 'Kayin', 'State').
	subdivision('mm-14', 'mm', 'Chin', 'State').
	subdivision('mm-15', 'mm', 'Mon', 'State').
	subdivision('mm-16', 'mm', 'Rakhine', 'State').
	subdivision('mm-17', 'mm', 'Shan', 'State').
	subdivision('mm-18', 'mm', 'Nay Pyi Taw', 'Union territory').
	subdivision('mn-035', 'mn', 'Orhon', 'Province').
	subdivision('mn-037', 'mn', 'Darhan uul', 'Province').
	subdivision('mn-039', 'mn', 'Hentiy', 'Province').
	subdivision('mn-041', 'mn', 'Hövsgöl', 'Province').
	subdivision('mn-043', 'mn', 'Hovd', 'Province').
	subdivision('mn-046', 'mn', 'Uvs', 'Province').
	subdivision('mn-047', 'mn', 'Töv', 'Province').
	subdivision('mn-049', 'mn', 'Selenge', 'Province').
	subdivision('mn-051', 'mn', 'Sühbaatar', 'Province').
	subdivision('mn-053', 'mn', 'Ömnögovĭ', 'Province').
	subdivision('mn-055', 'mn', 'Övörhangay', 'Province').
	subdivision('mn-057', 'mn', 'Dzavhan', 'Province').
	subdivision('mn-059', 'mn', 'Dundgovĭ', 'Province').
	subdivision('mn-061', 'mn', 'Dornod', 'Province').
	subdivision('mn-063', 'mn', 'Dornogovĭ', 'Province').
	subdivision('mn-064', 'mn', 'Govĭ-Sümber', 'Province').
	subdivision('mn-065', 'mn', 'Govĭ-Altay', 'Province').
	subdivision('mn-067', 'mn', 'Bulgan', 'Province').
	subdivision('mn-069', 'mn', 'Bayanhongor', 'Province').
	subdivision('mn-071', 'mn', 'Bayan-Ölgiy', 'Province').
	subdivision('mn-073', 'mn', 'Arhangay', 'Province').
	subdivision('mn-1', 'mn', 'Ulaanbaatar', 'Capital city').
	subdivision('mr-01', 'mr', 'Hodh ech Chargui', 'Region').
	subdivision('mr-02', 'mr', 'Hodh el Gharbi', 'Region').
	subdivision('mr-03', 'mr', 'Assaba', 'Region').
	subdivision('mr-04', 'mr', 'Gorgol', 'Region').
	subdivision('mr-05', 'mr', 'Brakna', 'Region').
	subdivision('mr-06', 'mr', 'Trarza', 'Region').
	subdivision('mr-07', 'mr', 'Adrar', 'Region').
	subdivision('mr-08', 'mr', 'Dakhlet Nouâdhibou', 'Region').
	subdivision('mr-09', 'mr', 'Tagant', 'Region').
	subdivision('mr-10', 'mr', 'Guidimaka', 'Region').
	subdivision('mr-11', 'mr', 'Tiris Zemmour', 'Region').
	subdivision('mr-12', 'mr', 'Inchiri', 'Region').
	subdivision('mr-13', 'mr', 'Nouakchott Ouest', 'Region').
	subdivision('mr-14', 'mr', 'Nouakchott Nord', 'Region').
	subdivision('mr-15', 'mr', 'Nouakchott Sud', 'Region').
	subdivision('mt-01', 'mt', 'Attard', 'Local council').
	subdivision('mt-02', 'mt', 'Balzan', 'Local council').
	subdivision('mt-03', 'mt', 'Birgu', 'Local council').
	subdivision('mt-04', 'mt', 'Birkirkara', 'Local council').
	subdivision('mt-05', 'mt', 'Birżebbuġa', 'Local council').
	subdivision('mt-06', 'mt', 'Bormla', 'Local council').
	subdivision('mt-07', 'mt', 'Dingli', 'Local council').
	subdivision('mt-08', 'mt', 'Fgura', 'Local council').
	subdivision('mt-09', 'mt', 'Floriana', 'Local council').
	subdivision('mt-10', 'mt', 'Fontana', 'Local council').
	subdivision('mt-11', 'mt', 'Gudja', 'Local council').
	subdivision('mt-12', 'mt', 'Gżira', 'Local council').
	subdivision('mt-13', 'mt', 'Għajnsielem', 'Local council').
	subdivision('mt-14', 'mt', 'Għarb', 'Local council').
	subdivision('mt-15', 'mt', 'Għargħur', 'Local council').
	subdivision('mt-16', 'mt', 'Għasri', 'Local council').
	subdivision('mt-17', 'mt', 'Għaxaq', 'Local council').
	subdivision('mt-18', 'mt', 'Ħamrun', 'Local council').
	subdivision('mt-19', 'mt', 'Iklin', 'Local council').
	subdivision('mt-20', 'mt', 'Isla', 'Local council').
	subdivision('mt-21', 'mt', 'Kalkara', 'Local council').
	subdivision('mt-22', 'mt', 'Kerċem', 'Local council').
	subdivision('mt-23', 'mt', 'Kirkop', 'Local council').
	subdivision('mt-24', 'mt', 'Lija', 'Local council').
	subdivision('mt-25', 'mt', 'Luqa', 'Local council').
	subdivision('mt-26', 'mt', 'Marsa', 'Local council').
	subdivision('mt-27', 'mt', 'Marsaskala', 'Local council').
	subdivision('mt-28', 'mt', 'Marsaxlokk', 'Local council').
	subdivision('mt-29', 'mt', 'Mdina', 'Local council').
	subdivision('mt-30', 'mt', 'Mellieħa', 'Local council').
	subdivision('mt-31', 'mt', 'Mġarr', 'Local council').
	subdivision('mt-32', 'mt', 'Mosta', 'Local council').
	subdivision('mt-33', 'mt', 'Mqabba', 'Local council').
	subdivision('mt-34', 'mt', 'Msida', 'Local council').
	subdivision('mt-35', 'mt', 'Mtarfa', 'Local council').
	subdivision('mt-36', 'mt', 'Munxar', 'Local council').
	subdivision('mt-37', 'mt', 'Nadur', 'Local council').
	subdivision('mt-38', 'mt', 'Naxxar', 'Local council').
	subdivision('mt-39', 'mt', 'Paola', 'Local council').
	subdivision('mt-40', 'mt', 'Pembroke', 'Local council').
	subdivision('mt-41', 'mt', 'Pietà', 'Local council').
	subdivision('mt-42', 'mt', 'Qala', 'Local council').
	subdivision('mt-43', 'mt', 'Qormi', 'Local council').
	subdivision('mt-44', 'mt', 'Qrendi', 'Local council').
	subdivision('mt-45', 'mt', 'Rabat Gozo', 'Local council').
	subdivision('mt-46', 'mt', 'Rabat Malta', 'Local council').
	subdivision('mt-47', 'mt', 'Safi', 'Local council').
	subdivision('mt-48', 'mt', 'Saint Julian''s', 'Local council').
	subdivision('mt-49', 'mt', 'Saint John', 'Local council').
	subdivision('mt-50', 'mt', 'Saint Lawrence', 'Local council').
	subdivision('mt-51', 'mt', 'Saint Paul''s Bay', 'Local council').
	subdivision('mt-52', 'mt', 'Sannat', 'Local council').
	subdivision('mt-53', 'mt', 'Saint Lucia''s', 'Local council').
	subdivision('mt-54', 'mt', 'Santa Venera', 'Local council').
	subdivision('mt-55', 'mt', 'Siġġiewi', 'Local council').
	subdivision('mt-56', 'mt', 'Sliema', 'Local council').
	subdivision('mt-57', 'mt', 'Swieqi', 'Local council').
	subdivision('mt-58', 'mt', 'Ta'' Xbiex', 'Local council').
	subdivision('mt-59', 'mt', 'Tarxien', 'Local council').
	subdivision('mt-60', 'mt', 'Valletta', 'Local council').
	subdivision('mt-61', 'mt', 'Xagħra', 'Local council').
	subdivision('mt-62', 'mt', 'Xewkija', 'Local council').
	subdivision('mt-63', 'mt', 'Xgħajra', 'Local council').
	subdivision('mt-64', 'mt', 'Żabbar', 'Local council').
	subdivision('mt-65', 'mt', 'Żebbuġ Gozo', 'Local council').
	subdivision('mt-66', 'mt', 'Żebbuġ Malta', 'Local council').
	subdivision('mt-67', 'mt', 'Żejtun', 'Local council').
	subdivision('mt-68', 'mt', 'Żurrieq', 'Local council').
	subdivision('mu-ag', 'mu', 'Agalega Islands', 'Dependency').
	subdivision('mu-bl', 'mu', 'Black River', 'District').
	subdivision('mu-cc', 'mu', 'Cargados Carajos Shoals', 'Dependency').
	subdivision('mu-fl', 'mu', 'Flacq', 'District').
	subdivision('mu-gp', 'mu', 'Grand Port', 'District').
	subdivision('mu-mo', 'mu', 'Moka', 'District').
	subdivision('mu-pa', 'mu', 'Pamplemousses', 'District').
	subdivision('mu-pl', 'mu', 'Port Louis', 'District').
	subdivision('mu-pw', 'mu', 'Plaines Wilhems', 'District').
	subdivision('mu-ro', 'mu', 'Rodrigues Island', 'Dependency').
	subdivision('mu-rr', 'mu', 'Rivière du Rempart', 'District').
	subdivision('mu-sa', 'mu', 'Savanne', 'District').
	subdivision('mv-00', 'mv', 'South Ari Atoll', 'Administrative atoll').
	subdivision('mv-01', 'mv', 'Addu City', 'City').
	subdivision('mv-02', 'mv', 'North Ari Atoll', 'Administrative atoll').
	subdivision('mv-03', 'mv', 'Faadhippolhu', 'Administrative atoll').
	subdivision('mv-04', 'mv', 'Felidhu Atoll', 'Administrative atoll').
	subdivision('mv-05', 'mv', 'Hahdhunmathi', 'Administrative atoll').
	subdivision('mv-07', 'mv', 'North Thiladhunmathi', 'Administrative atoll').
	subdivision('mv-08', 'mv', 'Kolhumadulu', 'Administrative atoll').
	subdivision('mv-12', 'mv', 'Mulaku Atoll', 'Administrative atoll').
	subdivision('mv-13', 'mv', 'North Maalhosmadulu', 'Administrative atoll').
	subdivision('mv-14', 'mv', 'North Nilandhe Atoll', 'Administrative atoll').
	subdivision('mv-17', 'mv', 'South Nilandhe Atoll', 'Administrative atoll').
	subdivision('mv-20', 'mv', 'South Maalhosmadulu', 'Administrative atoll').
	subdivision('mv-23', 'mv', 'South Thiladhunmathi', 'Administrative atoll').
	subdivision('mv-24', 'mv', 'North Miladhunmadulu', 'Administrative atoll').
	subdivision('mv-25', 'mv', 'South Miladhunmadulu', 'Administrative atoll').
	subdivision('mv-26', 'mv', 'Male Atoll', 'Administrative atoll').
	subdivision('mv-27', 'mv', 'North Huvadhu Atoll', 'Administrative atoll').
	subdivision('mv-28', 'mv', 'South Huvadhu Atoll', 'Administrative atoll').
	subdivision('mv-29', 'mv', 'Fuvammulah', 'Administrative atoll').
	subdivision('mv-mle', 'mv', 'Male', 'City').
	subdivision('mw-ba', 'mw', 'Balaka', 'District').
	subdivision('mw-bl', 'mw', 'Blantyre', 'District').
	subdivision('mw-c', 'mw', 'Central Region', 'Region').
	subdivision('mw-ck', 'mw', 'Chikwawa', 'District').
	subdivision('mw-cr', 'mw', 'Chiradzulu', 'District').
	subdivision('mw-ct', 'mw', 'Chitipa', 'District').
	subdivision('mw-de', 'mw', 'Dedza', 'District').
	subdivision('mw-do', 'mw', 'Dowa', 'District').
	subdivision('mw-kr', 'mw', 'Karonga', 'District').
	subdivision('mw-ks', 'mw', 'Kasungu', 'District').
	subdivision('mw-li', 'mw', 'Lilongwe', 'District').
	subdivision('mw-lk', 'mw', 'Likoma', 'District').
	subdivision('mw-mc', 'mw', 'Mchinji', 'District').
	subdivision('mw-mg', 'mw', 'Mangochi', 'District').
	subdivision('mw-mh', 'mw', 'Machinga', 'District').
	subdivision('mw-mu', 'mw', 'Mulanje', 'District').
	subdivision('mw-mw', 'mw', 'Mwanza', 'District').
	subdivision('mw-mz', 'mw', 'Mzimba', 'District').
	subdivision('mw-n', 'mw', 'Northern Region', 'Region').
	subdivision('mw-nb', 'mw', 'Nkhata Bay', 'District').
	subdivision('mw-ne', 'mw', 'Neno', 'District').
	subdivision('mw-ni', 'mw', 'Ntchisi', 'District').
	subdivision('mw-nk', 'mw', 'Nkhotakota', 'District').
	subdivision('mw-ns', 'mw', 'Nsanje', 'District').
	subdivision('mw-nu', 'mw', 'Ntcheu', 'District').
	subdivision('mw-ph', 'mw', 'Phalombe', 'District').
	subdivision('mw-ru', 'mw', 'Rumphi', 'District').
	subdivision('mw-s', 'mw', 'Southern Region', 'Region').
	subdivision('mw-sa', 'mw', 'Salima', 'District').
	subdivision('mw-th', 'mw', 'Thyolo', 'District').
	subdivision('mw-zo', 'mw', 'Zomba', 'District').
	subdivision('mx-agu', 'mx', 'Aguascalientes', 'State').
	subdivision('mx-bcn', 'mx', 'Baja California', 'State').
	subdivision('mx-bcs', 'mx', 'Baja California Sur', 'State').
	subdivision('mx-cam', 'mx', 'Campeche', 'State').
	subdivision('mx-chh', 'mx', 'Chihuahua', 'State').
	subdivision('mx-chp', 'mx', 'Chiapas', 'State').
	subdivision('mx-cmx', 'mx', 'Ciudad de México', 'Federal entity').
	subdivision('mx-coa', 'mx', 'Coahuila de Zaragoza', 'State').
	subdivision('mx-col', 'mx', 'Colima', 'State').
	subdivision('mx-dur', 'mx', 'Durango', 'State').
	subdivision('mx-gro', 'mx', 'Guerrero', 'State').
	subdivision('mx-gua', 'mx', 'Guanajuato', 'State').
	subdivision('mx-hid', 'mx', 'Hidalgo', 'State').
	subdivision('mx-jal', 'mx', 'Jalisco', 'State').
	subdivision('mx-mex', 'mx', 'México', 'State').
	subdivision('mx-mic', 'mx', 'Michoacán de Ocampo', 'State').
	subdivision('mx-mor', 'mx', 'Morelos', 'State').
	subdivision('mx-nay', 'mx', 'Nayarit', 'State').
	subdivision('mx-nle', 'mx', 'Nuevo León', 'State').
	subdivision('mx-oax', 'mx', 'Oaxaca', 'State').
	subdivision('mx-pue', 'mx', 'Puebla', 'State').
	subdivision('mx-que', 'mx', 'Querétaro', 'State').
	subdivision('mx-roo', 'mx', 'Quintana Roo', 'State').
	subdivision('mx-sin', 'mx', 'Sinaloa', 'State').
	subdivision('mx-slp', 'mx', 'San Luis Potosí', 'State').
	subdivision('mx-son', 'mx', 'Sonora', 'State').
	subdivision('mx-tab', 'mx', 'Tabasco', 'State').
	subdivision('mx-tam', 'mx', 'Tamaulipas', 'State').
	subdivision('mx-tla', 'mx', 'Tlaxcala', 'State').
	subdivision('mx-ver', 'mx', 'Veracruz de Ignacio de la Llave', 'State').
	subdivision('mx-yuc', 'mx', 'Yucatán', 'State').
	subdivision('mx-zac', 'mx', 'Zacatecas', 'State').
	subdivision('my-01', 'my', 'Johor', 'State').
	subdivision('my-02', 'my', 'Kedah', 'State').
	subdivision('my-03', 'my', 'Kelantan', 'State').
	subdivision('my-04', 'my', 'Melaka', 'State').
	subdivision('my-05', 'my', 'Negeri Sembilan', 'State').
	subdivision('my-06', 'my', 'Pahang', 'State').
	subdivision('my-07', 'my', 'Pulau Pinang', 'State').
	subdivision('my-08', 'my', 'Perak', 'State').
	subdivision('my-09', 'my', 'Perlis', 'State').
	subdivision('my-10', 'my', 'Selangor', 'State').
	subdivision('my-11', 'my', 'Terengganu', 'State').
	subdivision('my-12', 'my', 'Sabah', 'State').
	subdivision('my-13', 'my', 'Sarawak', 'State').
	subdivision('my-14', 'my', 'Wilayah Persekutuan Kuala Lumpur', 'Federal territory').
	subdivision('my-15', 'my', 'Wilayah Persekutuan Labuan', 'Federal territory').
	subdivision('my-16', 'my', 'Wilayah Persekutuan Putrajaya', 'Federal territory').
	subdivision('mz-a', 'mz', 'Niassa', 'Province').
	subdivision('mz-b', 'mz', 'Manica', 'Province').
	subdivision('mz-g', 'mz', 'Gaza', 'Province').
	subdivision('mz-i', 'mz', 'Inhambane', 'Province').
	subdivision('mz-l', 'mz', 'Maputo', 'Province').
	subdivision('mz-mpm', 'mz', 'Maputo', 'City').
	subdivision('mz-n', 'mz', 'Nampula', 'Province').
	subdivision('mz-p', 'mz', 'Cabo Delgado', 'Province').
	subdivision('mz-q', 'mz', 'Zambézia', 'Province').
	subdivision('mz-s', 'mz', 'Sofala', 'Province').
	subdivision('mz-t', 'mz', 'Tete', 'Province').
	subdivision('na-ca', 'na', 'Zambezi', 'Region').
	subdivision('na-er', 'na', 'Erongo', 'Region').
	subdivision('na-ha', 'na', 'Hardap', 'Region').
	subdivision('na-ka', 'na', '//Karas', 'Region').
	subdivision('na-ke', 'na', 'Kavango East', 'Region').
	subdivision('na-kh', 'na', 'Khomas', 'Region').
	subdivision('na-ku', 'na', 'Kunene', 'Region').
	subdivision('na-kw', 'na', 'Kavango West', 'Region').
	subdivision('na-od', 'na', 'Otjozondjupa', 'Region').
	subdivision('na-oh', 'na', 'Omaheke', 'Region').
	subdivision('na-on', 'na', 'Oshana', 'Region').
	subdivision('na-os', 'na', 'Omusati', 'Region').
	subdivision('na-ot', 'na', 'Oshikoto', 'Region').
	subdivision('na-ow', 'na', 'Ohangwena', 'Region').
	subdivision('ne-1', 'ne', 'Agadez', 'Region').
	subdivision('ne-2', 'ne', 'Diffa', 'Region').
	subdivision('ne-3', 'ne', 'Dosso', 'Region').
	subdivision('ne-4', 'ne', 'Maradi', 'Region').
	subdivision('ne-5', 'ne', 'Tahoua', 'Region').
	subdivision('ne-6', 'ne', 'Tillabéri', 'Region').
	subdivision('ne-7', 'ne', 'Zinder', 'Region').
	subdivision('ne-8', 'ne', 'Niamey', 'Urban community').
	subdivision('ng-ab', 'ng', 'Abia', 'State').
	subdivision('ng-ad', 'ng', 'Adamawa', 'State').
	subdivision('ng-ak', 'ng', 'Akwa Ibom', 'State').
	subdivision('ng-an', 'ng', 'Anambra', 'State').
	subdivision('ng-ba', 'ng', 'Bauchi', 'State').
	subdivision('ng-be', 'ng', 'Benue', 'State').
	subdivision('ng-bo', 'ng', 'Borno', 'State').
	subdivision('ng-by', 'ng', 'Bayelsa', 'State').
	subdivision('ng-cr', 'ng', 'Cross River', 'State').
	subdivision('ng-de', 'ng', 'Delta', 'State').
	subdivision('ng-eb', 'ng', 'Ebonyi', 'State').
	subdivision('ng-ed', 'ng', 'Edo', 'State').
	subdivision('ng-ek', 'ng', 'Ekiti', 'State').
	subdivision('ng-en', 'ng', 'Enugu', 'State').
	subdivision('ng-fc', 'ng', 'Abuja Federal Capital Territory', 'Capital territory').
	subdivision('ng-go', 'ng', 'Gombe', 'State').
	subdivision('ng-im', 'ng', 'Imo', 'State').
	subdivision('ng-ji', 'ng', 'Jigawa', 'State').
	subdivision('ng-kd', 'ng', 'Kaduna', 'State').
	subdivision('ng-ke', 'ng', 'Kebbi', 'State').
	subdivision('ng-kn', 'ng', 'Kano', 'State').
	subdivision('ng-ko', 'ng', 'Kogi', 'State').
	subdivision('ng-kt', 'ng', 'Katsina', 'State').
	subdivision('ng-kw', 'ng', 'Kwara', 'State').
	subdivision('ng-la', 'ng', 'Lagos', 'State').
	subdivision('ng-na', 'ng', 'Nasarawa', 'State').
	subdivision('ng-ni', 'ng', 'Niger', 'State').
	subdivision('ng-og', 'ng', 'Ogun', 'State').
	subdivision('ng-on', 'ng', 'Ondo', 'State').
	subdivision('ng-os', 'ng', 'Osun', 'State').
	subdivision('ng-oy', 'ng', 'Oyo', 'State').
	subdivision('ng-pl', 'ng', 'Plateau', 'State').
	subdivision('ng-ri', 'ng', 'Rivers', 'State').
	subdivision('ng-so', 'ng', 'Sokoto', 'State').
	subdivision('ng-ta', 'ng', 'Taraba', 'State').
	subdivision('ng-yo', 'ng', 'Yobe', 'State').
	subdivision('ng-za', 'ng', 'Zamfara', 'State').
	subdivision('ni-an', 'ni', 'Costa Caribe Norte', 'Autonomous region').
	subdivision('ni-as', 'ni', 'Costa Caribe Sur', 'Autonomous region').
	subdivision('ni-bo', 'ni', 'Boaco', 'Department').
	subdivision('ni-ca', 'ni', 'Carazo', 'Department').
	subdivision('ni-ci', 'ni', 'Chinandega', 'Department').
	subdivision('ni-co', 'ni', 'Chontales', 'Department').
	subdivision('ni-es', 'ni', 'Estelí', 'Department').
	subdivision('ni-gr', 'ni', 'Granada', 'Department').
	subdivision('ni-ji', 'ni', 'Jinotega', 'Department').
	subdivision('ni-le', 'ni', 'León', 'Department').
	subdivision('ni-md', 'ni', 'Madriz', 'Department').
	subdivision('ni-mn', 'ni', 'Managua', 'Department').
	subdivision('ni-ms', 'ni', 'Masaya', 'Department').
	subdivision('ni-mt', 'ni', 'Matagalpa', 'Department').
	subdivision('ni-ns', 'ni', 'Nueva Segovia', 'Department').
	subdivision('ni-ri', 'ni', 'Rivas', 'Department').
	subdivision('ni-sj', 'ni', 'Río San Juan', 'Department').
	subdivision('nl-aw', 'nl', 'Aruba', 'Country').
	subdivision('nl-bq1', 'nl', 'Bonaire', 'Special municipality').
	subdivision('nl-bq2', 'nl', 'Saba', 'Special municipality').
	subdivision('nl-bq3', 'nl', 'Sint Eustatius', 'Special municipality').
	subdivision('nl-cw', 'nl', 'Curaçao', 'Country').
	subdivision('nl-dr', 'nl', 'Drenthe', 'Province').
	subdivision('nl-fl', 'nl', 'Flevoland', 'Province').
	subdivision('nl-fr', 'nl', 'Fryslân', 'Province').
	subdivision('nl-ge', 'nl', 'Gelderland', 'Province').
	subdivision('nl-gr', 'nl', 'Groningen', 'Province').
	subdivision('nl-li', 'nl', 'Limburg', 'Province').
	subdivision('nl-nb', 'nl', 'Noord-Brabant', 'Province').
	subdivision('nl-nh', 'nl', 'Noord-Holland', 'Province').
	subdivision('nl-ov', 'nl', 'Overijssel', 'Province').
	subdivision('nl-sx', 'nl', 'Sint Maarten', 'Country').
	subdivision('nl-ut', 'nl', 'Utrecht', 'Province').
	subdivision('nl-ze', 'nl', 'Zeeland', 'Province').
	subdivision('nl-zh', 'nl', 'Zuid-Holland', 'Province').
	subdivision('no-03', 'no', 'Oslo', 'County').
	subdivision('no-11', 'no', 'Rogaland', 'County').
	subdivision('no-15', 'no', 'Møre og Romsdal', 'County').
	subdivision('no-18', 'no', 'Nordland', 'County').
	subdivision('no-21', 'no', 'Svalbard (Arctic Region)', 'Arctic region').
	subdivision('no-22', 'no', 'Jan Mayen (Arctic Region)', 'Arctic region').
	subdivision('no-30', 'no', 'Viken', 'County').
	subdivision('no-34', 'no', 'Innlandet', 'County').
	subdivision('no-38', 'no', 'Vestfold og Telemark', 'County').
	subdivision('no-42', 'no', 'Agder', 'County').
	subdivision('no-46', 'no', 'Vestland', 'County').
	subdivision('no-50', 'no', 'Trøndelag', 'County').
	subdivision('no-54', 'no', 'Troms og Finnmark', 'County').
	subdivision('np-p1', 'np', 'Koshi', 'Province').
	subdivision('np-p2', 'np', 'Madhesh', 'Province').
	subdivision('np-p3', 'np', 'Bagmati', 'Province').
	subdivision('np-p4', 'np', 'Gandaki', 'Province').
	subdivision('np-p5', 'np', 'Lumbini', 'Province').
	subdivision('np-p6', 'np', 'Karnali', 'Province').
	subdivision('np-p7', 'np', 'Sudurpashchim', 'Province').
	subdivision('nr-01', 'nr', 'Aiwo', 'District').
	subdivision('nr-02', 'nr', 'Anabar', 'District').
	subdivision('nr-03', 'nr', 'Anetan', 'District').
	subdivision('nr-04', 'nr', 'Anibare', 'District').
	subdivision('nr-05', 'nr', 'Baitsi', 'District').
	subdivision('nr-06', 'nr', 'Boe', 'District').
	subdivision('nr-07', 'nr', 'Buada', 'District').
	subdivision('nr-08', 'nr', 'Denigomodu', 'District').
	subdivision('nr-09', 'nr', 'Ewa', 'District').
	subdivision('nr-10', 'nr', 'Ijuw', 'District').
	subdivision('nr-11', 'nr', 'Meneng', 'District').
	subdivision('nr-12', 'nr', 'Nibok', 'District').
	subdivision('nr-13', 'nr', 'Uaboe', 'District').
	subdivision('nr-14', 'nr', 'Yaren', 'District').
	subdivision('nz-auk', 'nz', 'Auckland', 'Region').
	subdivision('nz-bop', 'nz', 'Bay of Plenty', 'Region').
	subdivision('nz-can', 'nz', 'Canterbury', 'Region').
	subdivision('nz-cit', 'nz', 'Chatham Islands Territory', 'Special island authority').
	subdivision('nz-gis', 'nz', 'Gisborne', 'Region').
	subdivision('nz-hkb', 'nz', 'Hawke''s Bay', 'Region').
	subdivision('nz-mbh', 'nz', 'Marlborough', 'Region').
	subdivision('nz-mwt', 'nz', 'Manawatū-Whanganui', 'Region').
	subdivision('nz-nsn', 'nz', 'Nelson', 'Region').
	subdivision('nz-ntl', 'nz', 'Northland', 'Region').
	subdivision('nz-ota', 'nz', 'Otago', 'Region').
	subdivision('nz-stl', 'nz', 'Southland', 'Region').
	subdivision('nz-tas', 'nz', 'Tasman', 'Region').
	subdivision('nz-tki', 'nz', 'Taranaki', 'Region').
	subdivision('nz-wgn', 'nz', 'Greater Wellington', 'Region').
	subdivision('nz-wko', 'nz', 'Waikato', 'Region').
	subdivision('nz-wtc', 'nz', 'West Coast', 'Region').
	subdivision('om-bj', 'om', 'Janūb al Bāţinah', 'Governorate').
	subdivision('om-bs', 'om', 'Shamāl al Bāţinah', 'Governorate').
	subdivision('om-bu', 'om', 'Al Buraymī', 'Governorate').
	subdivision('om-da', 'om', 'Ad Dākhilīyah', 'Governorate').
	subdivision('om-ma', 'om', 'Masqaţ', 'Governorate').
	subdivision('om-mu', 'om', 'Musandam', 'Governorate').
	subdivision('om-sj', 'om', 'Janūb ash Sharqīyah', 'Governorate').
	subdivision('om-ss', 'om', 'Shamāl ash Sharqīyah', 'Governorate').
	subdivision('om-wu', 'om', 'Al Wusţá', 'Governorate').
	subdivision('om-za', 'om', 'Az̧ Z̧āhirah', 'Governorate').
	subdivision('om-zu', 'om', 'Z̧ufār', 'Governorate').
	subdivision('pa-1', 'pa', 'Bocas del Toro', 'Province').
	subdivision('pa-10', 'pa', 'Panamá Oeste', 'Province').
	subdivision('pa-2', 'pa', 'Coclé', 'Province').
	subdivision('pa-3', 'pa', 'Colón', 'Province').
	subdivision('pa-4', 'pa', 'Chiriquí', 'Province').
	subdivision('pa-5', 'pa', 'Darién', 'Province').
	subdivision('pa-6', 'pa', 'Herrera', 'Province').
	subdivision('pa-7', 'pa', 'Los Santos', 'Province').
	subdivision('pa-8', 'pa', 'Panamá', 'Province').
	subdivision('pa-9', 'pa', 'Veraguas', 'Province').
	subdivision('pa-em', 'pa', 'Emberá', 'Indigenous region').
	subdivision('pa-ky', 'pa', 'Guna Yala', 'Indigenous region').
	subdivision('pa-nb', 'pa', 'Ngäbe-Buglé', 'Indigenous region').
	subdivision('pa-nt', 'pa', 'Naso Tjër Di', 'Indigenous region').
	subdivision('pe-ama', 'pe', 'Amazonas', 'Region').
	subdivision('pe-anc', 'pe', 'Ancash', 'Region').
	subdivision('pe-apu', 'pe', 'Apurímac', 'Region').
	subdivision('pe-are', 'pe', 'Arequipa', 'Region').
	subdivision('pe-aya', 'pe', 'Ayacucho', 'Region').
	subdivision('pe-caj', 'pe', 'Cajamarca', 'Region').
	subdivision('pe-cal', 'pe', 'El Callao', 'Region').
	subdivision('pe-cus', 'pe', 'Cusco', 'Region').
	subdivision('pe-huc', 'pe', 'Huánuco', 'Region').
	subdivision('pe-huv', 'pe', 'Huancavelica', 'Region').
	subdivision('pe-ica', 'pe', 'Ica', 'Region').
	subdivision('pe-jun', 'pe', 'Junín', 'Region').
	subdivision('pe-lal', 'pe', 'La Libertad', 'Region').
	subdivision('pe-lam', 'pe', 'Lambayeque', 'Region').
	subdivision('pe-lim', 'pe', 'Lima', 'Region').
	subdivision('pe-lma', 'pe', 'Municipalidad Metropolitana de Lima', 'Municipality').
	subdivision('pe-lor', 'pe', 'Loreto', 'Region').
	subdivision('pe-mdd', 'pe', 'Madre de Dios', 'Region').
	subdivision('pe-moq', 'pe', 'Moquegua', 'Region').
	subdivision('pe-pas', 'pe', 'Pasco', 'Region').
	subdivision('pe-piu', 'pe', 'Piura', 'Region').
	subdivision('pe-pun', 'pe', 'Puno', 'Region').
	subdivision('pe-sam', 'pe', 'San Martín', 'Region').
	subdivision('pe-tac', 'pe', 'Tacna', 'Region').
	subdivision('pe-tum', 'pe', 'Tumbes', 'Region').
	subdivision('pe-uca', 'pe', 'Ucayali', 'Region').
	subdivision('pg-cpk', 'pg', 'Chimbu', 'Province').
	subdivision('pg-cpm', 'pg', 'Central', 'Province').
	subdivision('pg-ebr', 'pg', 'East New Britain', 'Province').
	subdivision('pg-ehg', 'pg', 'Eastern Highlands', 'Province').
	subdivision('pg-epw', 'pg', 'Enga', 'Province').
	subdivision('pg-esw', 'pg', 'East Sepik', 'Province').
	subdivision('pg-gpk', 'pg', 'Gulf', 'Province').
	subdivision('pg-hla', 'pg', 'Hela', 'Province').
	subdivision('pg-jwk', 'pg', 'Jiwaka', 'Province').
	subdivision('pg-mba', 'pg', 'Milne Bay', 'Province').
	subdivision('pg-mpl', 'pg', 'Morobe', 'Province').
	subdivision('pg-mpm', 'pg', 'Madang', 'Province').
	subdivision('pg-mrl', 'pg', 'Manus', 'Province').
	subdivision('pg-ncd', 'pg', 'National Capital District (Port Moresby)', 'District').
	subdivision('pg-nik', 'pg', 'New Ireland', 'Province').
	subdivision('pg-npp', 'pg', 'Northern', 'Province').
	subdivision('pg-nsb', 'pg', 'Bougainville', 'Autonomous region').
	subdivision('pg-san', 'pg', 'West Sepik', 'Province').
	subdivision('pg-shm', 'pg', 'Southern Highlands', 'Province').
	subdivision('pg-wbk', 'pg', 'West New Britain', 'Province').
	subdivision('pg-whm', 'pg', 'Western Highlands', 'Province').
	subdivision('pg-wpd', 'pg', 'Western', 'Province').
	subdivision('ph-00', 'ph', 'National Capital Region', 'Region').
	subdivision('ph-01', 'ph', 'Ilocos (Region I)', 'Region').
	subdivision('ph-02', 'ph', 'Cagayan Valley (Region II)', 'Region').
	subdivision('ph-03', 'ph', 'Central Luzon (Region III)', 'Region').
	subdivision('ph-05', 'ph', 'Bicol (Region V)', 'Region').
	subdivision('ph-06', 'ph', 'Western Visayas (Region VI)', 'Region').
	subdivision('ph-07', 'ph', 'Central Visayas (Region VII)', 'Region').
	subdivision('ph-08', 'ph', 'Eastern Visayas (Region VIII)', 'Region').
	subdivision('ph-09', 'ph', 'Zamboanga Peninsula (Region IX)', 'Region').
	subdivision('ph-10', 'ph', 'Northern Mindanao (Region X)', 'Region').
	subdivision('ph-11', 'ph', 'Davao (Region XI)', 'Region').
	subdivision('ph-12', 'ph', 'Soccsksargen (Region XII)', 'Region').
	subdivision('ph-13', 'ph', 'Caraga (Region XIII)', 'Region').
	subdivision('ph-14', 'ph', 'Autonomous Region in Muslim Mindanao (ARMM)', 'Region').
	subdivision('ph-15', 'ph', 'Cordillera Administrative Region (CAR)', 'Region').
	subdivision('ph-40', 'ph', 'Calabarzon (Region IV-A)', 'Region').
	subdivision('ph-41', 'ph', 'Mimaropa (Region IV-B)', 'Region').
	subdivision('ph-abr', 'ph', 'Abra', 'Province').
	subdivision('ph-agn', 'ph', 'Agusan del Norte', 'Province').
	subdivision('ph-ags', 'ph', 'Agusan del Sur', 'Province').
	subdivision('ph-akl', 'ph', 'Aklan', 'Province').
	subdivision('ph-alb', 'ph', 'Albay', 'Province').
	subdivision('ph-ant', 'ph', 'Antique', 'Province').
	subdivision('ph-apa', 'ph', 'Apayao', 'Province').
	subdivision('ph-aur', 'ph', 'Aurora', 'Province').
	subdivision('ph-ban', 'ph', 'Bataan', 'Province').
	subdivision('ph-bas', 'ph', 'Basilan', 'Province').
	subdivision('ph-ben', 'ph', 'Benguet', 'Province').
	subdivision('ph-bil', 'ph', 'Biliran', 'Province').
	subdivision('ph-boh', 'ph', 'Bohol', 'Province').
	subdivision('ph-btg', 'ph', 'Batangas', 'Province').
	subdivision('ph-btn', 'ph', 'Batanes', 'Province').
	subdivision('ph-buk', 'ph', 'Bukidnon', 'Province').
	subdivision('ph-bul', 'ph', 'Bulacan', 'Province').
	subdivision('ph-cag', 'ph', 'Cagayan', 'Province').
	subdivision('ph-cam', 'ph', 'Camiguin', 'Province').
	subdivision('ph-can', 'ph', 'Camarines Norte', 'Province').
	subdivision('ph-cap', 'ph', 'Capiz', 'Province').
	subdivision('ph-cas', 'ph', 'Camarines Sur', 'Province').
	subdivision('ph-cat', 'ph', 'Catanduanes', 'Province').
	subdivision('ph-cav', 'ph', 'Cavite', 'Province').
	subdivision('ph-ceb', 'ph', 'Cebu', 'Province').
	subdivision('ph-com', 'ph', 'Davao de Oro', 'Province').
	subdivision('ph-dao', 'ph', 'Davao Oriental', 'Province').
	subdivision('ph-das', 'ph', 'Davao del Sur', 'Province').
	subdivision('ph-dav', 'ph', 'Davao del Norte', 'Province').
	subdivision('ph-din', 'ph', 'Dinagat Islands', 'Province').
	subdivision('ph-dvo', 'ph', 'Davao Occidental', 'Province').
	subdivision('ph-eas', 'ph', 'Eastern Samar', 'Province').
	subdivision('ph-gui', 'ph', 'Guimaras', 'Province').
	subdivision('ph-ifu', 'ph', 'Ifugao', 'Province').
	subdivision('ph-ili', 'ph', 'Iloilo', 'Province').
	subdivision('ph-iln', 'ph', 'Ilocos Norte', 'Province').
	subdivision('ph-ils', 'ph', 'Ilocos Sur', 'Province').
	subdivision('ph-isa', 'ph', 'Isabela', 'Province').
	subdivision('ph-kal', 'ph', 'Kalinga', 'Province').
	subdivision('ph-lag', 'ph', 'Laguna', 'Province').
	subdivision('ph-lan', 'ph', 'Lanao del Norte', 'Province').
	subdivision('ph-las', 'ph', 'Lanao del Sur', 'Province').
	subdivision('ph-ley', 'ph', 'Leyte', 'Province').
	subdivision('ph-lun', 'ph', 'La Union', 'Province').
	subdivision('ph-mad', 'ph', 'Marinduque', 'Province').
	subdivision('ph-mas', 'ph', 'Masbate', 'Province').
	subdivision('ph-mdc', 'ph', 'Mindoro Occidental', 'Province').
	subdivision('ph-mdr', 'ph', 'Mindoro Oriental', 'Province').
	subdivision('ph-mgn', 'ph', 'Maguindanao del Norte', 'Province').
	subdivision('ph-mgs', 'ph', 'Maguindanao del Sur', 'Province').
	subdivision('ph-mou', 'ph', 'Mountain Province', 'Province').
	subdivision('ph-msc', 'ph', 'Misamis Occidental', 'Province').
	subdivision('ph-msr', 'ph', 'Misamis Oriental', 'Province').
	subdivision('ph-nco', 'ph', 'Cotabato', 'Province').
	subdivision('ph-nec', 'ph', 'Negros Occidental', 'Province').
	subdivision('ph-ner', 'ph', 'Negros Oriental', 'Province').
	subdivision('ph-nsa', 'ph', 'Northern Samar', 'Province').
	subdivision('ph-nue', 'ph', 'Nueva Ecija', 'Province').
	subdivision('ph-nuv', 'ph', 'Nueva Vizcaya', 'Province').
	subdivision('ph-pam', 'ph', 'Pampanga', 'Province').
	subdivision('ph-pan', 'ph', 'Pangasinan', 'Province').
	subdivision('ph-plw', 'ph', 'Palawan', 'Province').
	subdivision('ph-que', 'ph', 'Quezon', 'Province').
	subdivision('ph-qui', 'ph', 'Quirino', 'Province').
	subdivision('ph-riz', 'ph', 'Rizal', 'Province').
	subdivision('ph-rom', 'ph', 'Romblon', 'Province').
	subdivision('ph-sar', 'ph', 'Sarangani', 'Province').
	subdivision('ph-sco', 'ph', 'South Cotabato', 'Province').
	subdivision('ph-sig', 'ph', 'Siquijor', 'Province').
	subdivision('ph-sle', 'ph', 'Southern Leyte', 'Province').
	subdivision('ph-slu', 'ph', 'Sulu', 'Province').
	subdivision('ph-sor', 'ph', 'Sorsogon', 'Province').
	subdivision('ph-suk', 'ph', 'Sultan Kudarat', 'Province').
	subdivision('ph-sun', 'ph', 'Surigao del Norte', 'Province').
	subdivision('ph-sur', 'ph', 'Surigao del Sur', 'Province').
	subdivision('ph-tar', 'ph', 'Tarlac', 'Province').
	subdivision('ph-taw', 'ph', 'Tawi-Tawi', 'Province').
	subdivision('ph-wsa', 'ph', 'Samar', 'Province').
	subdivision('ph-zan', 'ph', 'Zamboanga del Norte', 'Province').
	subdivision('ph-zas', 'ph', 'Zamboanga del Sur', 'Province').
	subdivision('ph-zmb', 'ph', 'Zambales', 'Province').
	subdivision('ph-zsi', 'ph', 'Zamboanga Sibugay', 'Province').
	subdivision('pk-ba', 'pk', 'Balochistan', 'Province').
	subdivision('pk-gb', 'pk', 'Gilgit-Baltistan', 'Pakistan administered area').
	subdivision('pk-is', 'pk', 'Islamabad', 'Federal capital territory').
	subdivision('pk-jk', 'pk', 'Azad Jammu and Kashmir', 'Pakistan administered area').
	subdivision('pk-kp', 'pk', 'Khyber Pakhtunkhwa', 'Province').
	subdivision('pk-pb', 'pk', 'Punjab', 'Province').
	subdivision('pk-sd', 'pk', 'Sindh', 'Province').
	subdivision('pl-02', 'pl', 'Dolnośląskie', 'Voivodship').
	subdivision('pl-04', 'pl', 'Kujawsko-Pomorskie', 'Voivodship').
	subdivision('pl-06', 'pl', 'Lubelskie', 'Voivodship').
	subdivision('pl-08', 'pl', 'Lubuskie', 'Voivodship').
	subdivision('pl-10', 'pl', 'Łódzkie', 'Voivodship').
	subdivision('pl-12', 'pl', 'Małopolskie', 'Voivodship').
	subdivision('pl-14', 'pl', 'Mazowieckie', 'Voivodship').
	subdivision('pl-16', 'pl', 'Opolskie', 'Voivodship').
	subdivision('pl-18', 'pl', 'Podkarpackie', 'Voivodship').
	subdivision('pl-20', 'pl', 'Podlaskie', 'Voivodship').
	subdivision('pl-22', 'pl', 'Pomorskie', 'Voivodship').
	subdivision('pl-24', 'pl', 'Śląskie', 'Voivodship').
	subdivision('pl-26', 'pl', 'Świętokrzyskie', 'Voivodship').
	subdivision('pl-28', 'pl', 'Warmińsko-Mazurskie', 'Voivodship').
	subdivision('pl-30', 'pl', 'Wielkopolskie', 'Voivodship').
	subdivision('pl-32', 'pl', 'Zachodniopomorskie', 'Voivodship').
	subdivision('ps-bth', 'ps', 'Bethlehem', 'Governorate').
	subdivision('ps-deb', 'ps', 'Deir El Balah', 'Governorate').
	subdivision('ps-gza', 'ps', 'Gaza', 'Governorate').
	subdivision('ps-hbn', 'ps', 'Hebron', 'Governorate').
	subdivision('ps-jem', 'ps', 'Jerusalem', 'Governorate').
	subdivision('ps-jen', 'ps', 'Jenin', 'Governorate').
	subdivision('ps-jrh', 'ps', 'Jericho and Al Aghwar', 'Governorate').
	subdivision('ps-kys', 'ps', 'Khan Yunis', 'Governorate').
	subdivision('ps-nbs', 'ps', 'Nablus', 'Governorate').
	subdivision('ps-ngz', 'ps', 'North Gaza', 'Governorate').
	subdivision('ps-qqa', 'ps', 'Qalqilya', 'Governorate').
	subdivision('ps-rbh', 'ps', 'Ramallah', 'Governorate').
	subdivision('ps-rfh', 'ps', 'Rafah', 'Governorate').
	subdivision('ps-slt', 'ps', 'Salfit', 'Governorate').
	subdivision('ps-tbs', 'ps', 'Tubas', 'Governorate').
	subdivision('ps-tkm', 'ps', 'Tulkarm', 'Governorate').
	subdivision('pt-01', 'pt', 'Aveiro', 'District').
	subdivision('pt-02', 'pt', 'Beja', 'District').
	subdivision('pt-03', 'pt', 'Braga', 'District').
	subdivision('pt-04', 'pt', 'Bragança', 'District').
	subdivision('pt-05', 'pt', 'Castelo Branco', 'District').
	subdivision('pt-06', 'pt', 'Coimbra', 'District').
	subdivision('pt-07', 'pt', 'Évora', 'District').
	subdivision('pt-08', 'pt', 'Faro', 'District').
	subdivision('pt-09', 'pt', 'Guarda', 'District').
	subdivision('pt-10', 'pt', 'Leiria', 'District').
	subdivision('pt-11', 'pt', 'Lisboa', 'District').
	subdivision('pt-12', 'pt', 'Portalegre', 'District').
	subdivision('pt-13', 'pt', 'Porto', 'District').
	subdivision('pt-14', 'pt', 'Santarém', 'District').
	subdivision('pt-15', 'pt', 'Setúbal', 'District').
	subdivision('pt-16', 'pt', 'Viana do Castelo', 'District').
	subdivision('pt-17', 'pt', 'Vila Real', 'District').
	subdivision('pt-18', 'pt', 'Viseu', 'District').
	subdivision('pt-20', 'pt', 'Região Autónoma dos Açores', 'Autonomous region').
	subdivision('pt-30', 'pt', 'Região Autónoma da Madeira', 'Autonomous region').
	subdivision('pw-002', 'pw', 'Aimeliik', 'State').
	subdivision('pw-004', 'pw', 'Airai', 'State').
	subdivision('pw-010', 'pw', 'Angaur', 'State').
	subdivision('pw-050', 'pw', 'Hatohobei', 'State').
	subdivision('pw-100', 'pw', 'Kayangel', 'State').
	subdivision('pw-150', 'pw', 'Koror', 'State').
	subdivision('pw-212', 'pw', 'Melekeok', 'State').
	subdivision('pw-214', 'pw', 'Ngaraard', 'State').
	subdivision('pw-218', 'pw', 'Ngarchelong', 'State').
	subdivision('pw-222', 'pw', 'Ngardmau', 'State').
	subdivision('pw-224', 'pw', 'Ngatpang', 'State').
	subdivision('pw-226', 'pw', 'Ngchesar', 'State').
	subdivision('pw-227', 'pw', 'Ngeremlengui', 'State').
	subdivision('pw-228', 'pw', 'Ngiwal', 'State').
	subdivision('pw-350', 'pw', 'Peleliu', 'State').
	subdivision('pw-370', 'pw', 'Sonsorol', 'State').
	subdivision('py-1', 'py', 'Concepción', 'Department').
	subdivision('py-10', 'py', 'Alto Paraná', 'Department').
	subdivision('py-11', 'py', 'Central', 'Department').
	subdivision('py-12', 'py', 'Ñeembucú', 'Department').
	subdivision('py-13', 'py', 'Amambay', 'Department').
	subdivision('py-14', 'py', 'Canindeyú', 'Department').
	subdivision('py-15', 'py', 'Presidente Hayes', 'Department').
	subdivision('py-16', 'py', 'Alto Paraguay', 'Department').
	subdivision('py-19', 'py', 'Boquerón', 'Department').
	subdivision('py-2', 'py', 'San Pedro', 'Department').
	subdivision('py-3', 'py', 'Cordillera', 'Department').
	subdivision('py-4', 'py', 'Guairá', 'Department').
	subdivision('py-5', 'py', 'Caaguazú', 'Department').
	subdivision('py-6', 'py', 'Caazapá', 'Department').
	subdivision('py-7', 'py', 'Itapúa', 'Department').
	subdivision('py-8', 'py', 'Misiones', 'Department').
	subdivision('py-9', 'py', 'Paraguarí', 'Department').
	subdivision('py-asu', 'py', 'Asunción', 'Capital').
	subdivision('qa-da', 'qa', 'Ad Dawḩah', 'Municipality').
	subdivision('qa-kh', 'qa', 'Al Khawr wa adh Dhakhīrah', 'Municipality').
	subdivision('qa-ms', 'qa', 'Ash Shamāl', 'Municipality').
	subdivision('qa-ra', 'qa', 'Ar Rayyān', 'Municipality').
	subdivision('qa-sh', 'qa', 'Ash Shīḩānīyah', 'Municipality').
	subdivision('qa-us', 'qa', 'Umm Şalāl', 'Municipality').
	subdivision('qa-wa', 'qa', 'Al Wakrah', 'Municipality').
	subdivision('qa-za', 'qa', 'Az̧ Z̧a‘āyin', 'Municipality').
	subdivision('ro-ab', 'ro', 'Alba', 'Department').
	subdivision('ro-ag', 'ro', 'Argeș', 'Department').
	subdivision('ro-ar', 'ro', 'Arad', 'Department').
	subdivision('ro-b', 'ro', 'București', 'Municipality').
	subdivision('ro-bc', 'ro', 'Bacău', 'Department').
	subdivision('ro-bh', 'ro', 'Bihor', 'Department').
	subdivision('ro-bn', 'ro', 'Bistrița-Năsăud', 'Department').
	subdivision('ro-br', 'ro', 'Brăila', 'Department').
	subdivision('ro-bt', 'ro', 'Botoșani', 'Department').
	subdivision('ro-bv', 'ro', 'Brașov', 'Department').
	subdivision('ro-bz', 'ro', 'Buzău', 'Department').
	subdivision('ro-cj', 'ro', 'Cluj', 'Department').
	subdivision('ro-cl', 'ro', 'Călărași', 'Department').
	subdivision('ro-cs', 'ro', 'Caraș-Severin', 'Department').
	subdivision('ro-ct', 'ro', 'Constanța', 'Department').
	subdivision('ro-cv', 'ro', 'Covasna', 'Department').
	subdivision('ro-db', 'ro', 'Dâmbovița', 'Department').
	subdivision('ro-dj', 'ro', 'Dolj', 'Department').
	subdivision('ro-gj', 'ro', 'Gorj', 'Department').
	subdivision('ro-gl', 'ro', 'Galați', 'Department').
	subdivision('ro-gr', 'ro', 'Giurgiu', 'Department').
	subdivision('ro-hd', 'ro', 'Hunedoara', 'Department').
	subdivision('ro-hr', 'ro', 'Harghita', 'Department').
	subdivision('ro-if', 'ro', 'Ilfov', 'Department').
	subdivision('ro-il', 'ro', 'Ialomița', 'Department').
	subdivision('ro-is', 'ro', 'Iași', 'Department').
	subdivision('ro-mh', 'ro', 'Mehedinți', 'Department').
	subdivision('ro-mm', 'ro', 'Maramureș', 'Department').
	subdivision('ro-ms', 'ro', 'Mureș', 'Department').
	subdivision('ro-nt', 'ro', 'Neamț', 'Department').
	subdivision('ro-ot', 'ro', 'Olt', 'Department').
	subdivision('ro-ph', 'ro', 'Prahova', 'Department').
	subdivision('ro-sb', 'ro', 'Sibiu', 'Department').
	subdivision('ro-sj', 'ro', 'Sălaj', 'Department').
	subdivision('ro-sm', 'ro', 'Satu Mare', 'Department').
	subdivision('ro-sv', 'ro', 'Suceava', 'Department').
	subdivision('ro-tl', 'ro', 'Tulcea', 'Department').
	subdivision('ro-tm', 'ro', 'Timiș', 'Department').
	subdivision('ro-tr', 'ro', 'Teleorman', 'Department').
	subdivision('ro-vl', 'ro', 'Vâlcea', 'Department').
	subdivision('ro-vn', 'ro', 'Vrancea', 'Department').
	subdivision('ro-vs', 'ro', 'Vaslui', 'Department').
	subdivision('rs-00', 'rs', 'Beograd', 'City').
	subdivision('rs-01', 'rs', 'Severnobački okrug', 'District').
	subdivision('rs-02', 'rs', 'Srednjebanatski okrug', 'District').
	subdivision('rs-03', 'rs', 'Severnobanatski okrug', 'District').
	subdivision('rs-04', 'rs', 'Južnobanatski okrug', 'District').
	subdivision('rs-05', 'rs', 'Zapadnobački okrug', 'District').
	subdivision('rs-06', 'rs', 'Južnobački okrug', 'District').
	subdivision('rs-07', 'rs', 'Sremski okrug', 'District').
	subdivision('rs-08', 'rs', 'Mačvanski okrug', 'District').
	subdivision('rs-09', 'rs', 'Kolubarski okrug', 'District').
	subdivision('rs-10', 'rs', 'Podunavski okrug', 'District').
	subdivision('rs-11', 'rs', 'Braničevski okrug', 'District').
	subdivision('rs-12', 'rs', 'Šumadijski okrug', 'District').
	subdivision('rs-13', 'rs', 'Pomoravski okrug', 'District').
	subdivision('rs-14', 'rs', 'Borski okrug', 'District').
	subdivision('rs-15', 'rs', 'Zaječarski okrug', 'District').
	subdivision('rs-16', 'rs', 'Zlatiborski okrug', 'District').
	subdivision('rs-17', 'rs', 'Moravički okrug', 'District').
	subdivision('rs-18', 'rs', 'Raški okrug', 'District').
	subdivision('rs-19', 'rs', 'Rasinski okrug', 'District').
	subdivision('rs-20', 'rs', 'Nišavski okrug', 'District').
	subdivision('rs-21', 'rs', 'Toplički okrug', 'District').
	subdivision('rs-22', 'rs', 'Pirotski okrug', 'District').
	subdivision('rs-23', 'rs', 'Jablanički okrug', 'District').
	subdivision('rs-24', 'rs', 'Pčinjski okrug', 'District').
	subdivision('rs-25', 'rs', 'Kosovski okrug', 'District').
	subdivision('rs-26', 'rs', 'Pećki okrug', 'District').
	subdivision('rs-27', 'rs', 'Prizrenski okrug', 'District').
	subdivision('rs-28', 'rs', 'Kosovsko-Mitrovački okrug', 'District').
	subdivision('rs-29', 'rs', 'Kosovsko-Pomoravski okrug', 'District').
	subdivision('rs-km', 'rs', 'Kosovo-Metohija', 'Autonomous province').
	subdivision('rs-vo', 'rs', 'Vojvodina', 'Autonomous province').
	subdivision('ru-ad', 'ru', 'Adygeya, Respublika', 'Republic').
	subdivision('ru-al', 'ru', 'Altay, Respublika', 'Republic').
	subdivision('ru-alt', 'ru', 'Altayskiy kray', 'Administrative territory').
	subdivision('ru-amu', 'ru', 'Amurskaya oblast''', 'Administrative region').
	subdivision('ru-ark', 'ru', 'Arkhangel''skaya oblast''', 'Administrative region').
	subdivision('ru-ast', 'ru', 'Astrakhanskaya oblast''', 'Administrative region').
	subdivision('ru-ba', 'ru', 'Bashkortostan, Respublika', 'Republic').
	subdivision('ru-bel', 'ru', 'Belgorodskaya oblast''', 'Administrative region').
	subdivision('ru-bry', 'ru', 'Bryanskaya oblast''', 'Administrative region').
	subdivision('ru-bu', 'ru', 'Buryatiya, Respublika', 'Republic').
	subdivision('ru-ce', 'ru', 'Chechenskaya Respublika', 'Republic').
	subdivision('ru-che', 'ru', 'Chelyabinskaya oblast''', 'Administrative region').
	subdivision('ru-chu', 'ru', 'Chukotskiy avtonomnyy okrug', 'Autonomous district').
	subdivision('ru-cu', 'ru', 'Chuvashskaya Respublika', 'Republic').
	subdivision('ru-da', 'ru', 'Dagestan, Respublika', 'Republic').
	subdivision('ru-in', 'ru', 'Ingushetiya, Respublika', 'Republic').
	subdivision('ru-irk', 'ru', 'Irkutskaya oblast''', 'Administrative region').
	subdivision('ru-iva', 'ru', 'Ivanovskaya oblast''', 'Administrative region').
	subdivision('ru-kam', 'ru', 'Kamchatskiy kray', 'Administrative territory').
	subdivision('ru-kb', 'ru', 'Kabardino-Balkarskaya Respublika', 'Republic').
	subdivision('ru-kc', 'ru', 'Karachayevo-Cherkesskaya Respublika', 'Republic').
	subdivision('ru-kda', 'ru', 'Krasnodarskiy kray', 'Administrative territory').
	subdivision('ru-kem', 'ru', 'Kemerovskaya oblast''', 'Administrative region').
	subdivision('ru-kgd', 'ru', 'Kaliningradskaya oblast''', 'Administrative region').
	subdivision('ru-kgn', 'ru', 'Kurganskaya oblast''', 'Administrative region').
	subdivision('ru-kha', 'ru', 'Khabarovskiy kray', 'Administrative territory').
	subdivision('ru-khm', 'ru', 'Khanty-Mansiyskiy avtonomnyy okrug', 'Autonomous district').
	subdivision('ru-kir', 'ru', 'Kirovskaya oblast''', 'Administrative region').
	subdivision('ru-kk', 'ru', 'Khakasiya, Respublika', 'Republic').
	subdivision('ru-kl', 'ru', 'Kalmykiya, Respublika', 'Republic').
	subdivision('ru-klu', 'ru', 'Kaluzhskaya oblast''', 'Administrative region').
	subdivision('ru-ko', 'ru', 'Komi, Respublika', 'Republic').
	subdivision('ru-kos', 'ru', 'Kostromskaya oblast''', 'Administrative region').
	subdivision('ru-kr', 'ru', 'Kareliya, Respublika', 'Republic').
	subdivision('ru-krs', 'ru', 'Kurskaya oblast''', 'Administrative region').
	subdivision('ru-kya', 'ru', 'Krasnoyarskiy kray', 'Administrative territory').
	subdivision('ru-len', 'ru', 'Leningradskaya oblast''', 'Administrative region').
	subdivision('ru-lip', 'ru', 'Lipetskaya oblast''', 'Administrative region').
	subdivision('ru-mag', 'ru', 'Magadanskaya oblast''', 'Administrative region').
	subdivision('ru-me', 'ru', 'Mariy El, Respublika', 'Republic').
	subdivision('ru-mo', 'ru', 'Mordoviya, Respublika', 'Republic').
	subdivision('ru-mos', 'ru', 'Moskovskaya oblast''', 'Administrative region').
	subdivision('ru-mow', 'ru', 'Moskva', 'Autonomous city').
	subdivision('ru-mur', 'ru', 'Murmanskaya oblast''', 'Administrative region').
	subdivision('ru-nen', 'ru', 'Nenetskiy avtonomnyy okrug', 'Autonomous district').
	subdivision('ru-ngr', 'ru', 'Novgorodskaya oblast''', 'Administrative region').
	subdivision('ru-niz', 'ru', 'Nizhegorodskaya oblast''', 'Administrative region').
	subdivision('ru-nvs', 'ru', 'Novosibirskaya oblast''', 'Administrative region').
	subdivision('ru-oms', 'ru', 'Omskaya oblast''', 'Administrative region').
	subdivision('ru-ore', 'ru', 'Orenburgskaya oblast''', 'Administrative region').
	subdivision('ru-orl', 'ru', 'Orlovskaya oblast''', 'Administrative region').
	subdivision('ru-per', 'ru', 'Permskiy kray', 'Administrative territory').
	subdivision('ru-pnz', 'ru', 'Penzenskaya oblast''', 'Administrative region').
	subdivision('ru-pri', 'ru', 'Primorskiy kray', 'Administrative territory').
	subdivision('ru-psk', 'ru', 'Pskovskaya oblast''', 'Administrative region').
	subdivision('ru-ros', 'ru', 'Rostovskaya oblast''', 'Administrative region').
	subdivision('ru-rya', 'ru', 'Ryazanskaya oblast''', 'Administrative region').
	subdivision('ru-sa', 'ru', 'Saha, Respublika', 'Republic').
	subdivision('ru-sak', 'ru', 'Sakhalinskaya oblast''', 'Administrative region').
	subdivision('ru-sam', 'ru', 'Samarskaya oblast''', 'Administrative region').
	subdivision('ru-sar', 'ru', 'Saratovskaya oblast''', 'Administrative region').
	subdivision('ru-se', 'ru', 'Severnaya Osetiya, Respublika', 'Republic').
	subdivision('ru-smo', 'ru', 'Smolenskaya oblast''', 'Administrative region').
	subdivision('ru-spe', 'ru', 'Sankt-Peterburg', 'Autonomous city').
	subdivision('ru-sta', 'ru', 'Stavropol''skiy kray', 'Administrative territory').
	subdivision('ru-sve', 'ru', 'Sverdlovskaya oblast''', 'Administrative region').
	subdivision('ru-ta', 'ru', 'Tatarstan, Respublika', 'Republic').
	subdivision('ru-tam', 'ru', 'Tambovskaya oblast''', 'Administrative region').
	subdivision('ru-tom', 'ru', 'Tomskaya oblast''', 'Administrative region').
	subdivision('ru-tul', 'ru', 'Tul''skaya oblast''', 'Administrative region').
	subdivision('ru-tve', 'ru', 'Tverskaya oblast''', 'Administrative region').
	subdivision('ru-ty', 'ru', 'Tyva, Respublika', 'Republic').
	subdivision('ru-tyu', 'ru', 'Tyumenskaya oblast''', 'Administrative region').
	subdivision('ru-ud', 'ru', 'Udmurtskaya Respublika', 'Republic').
	subdivision('ru-uly', 'ru', 'Ul''yanovskaya oblast''', 'Administrative region').
	subdivision('ru-vgg', 'ru', 'Volgogradskaya oblast''', 'Administrative region').
	subdivision('ru-vla', 'ru', 'Vladimirskaya oblast''', 'Administrative region').
	subdivision('ru-vlg', 'ru', 'Vologodskaya oblast''', 'Administrative region').
	subdivision('ru-vor', 'ru', 'Voronezhskaya oblast''', 'Administrative region').
	subdivision('ru-yan', 'ru', 'Yamalo-Nenetskiy avtonomnyy okrug', 'Autonomous district').
	subdivision('ru-yar', 'ru', 'Yaroslavskaya oblast''', 'Administrative region').
	subdivision('ru-yev', 'ru', 'Yevreyskaya avtonomnaya oblast''', 'Autonomous region').
	subdivision('ru-zab', 'ru', 'Zabaykal''skiy kray', 'Administrative territory').
	subdivision('rw-01', 'rw', 'City of Kigali', 'City').
	subdivision('rw-02', 'rw', 'Eastern', 'Province').
	subdivision('rw-03', 'rw', 'Northern', 'Province').
	subdivision('rw-04', 'rw', 'Western', 'Province').
	subdivision('rw-05', 'rw', 'Southern', 'Province').
	subdivision('sa-01', 'sa', 'Ar Riyāḑ', 'Region').
	subdivision('sa-02', 'sa', 'Makkah al Mukarramah', 'Region').
	subdivision('sa-03', 'sa', 'Al Madīnah al Munawwarah', 'Region').
	subdivision('sa-04', 'sa', 'Ash Sharqīyah', 'Region').
	subdivision('sa-05', 'sa', 'Al Qaşīm', 'Region').
	subdivision('sa-06', 'sa', 'Ḩā''il', 'Region').
	subdivision('sa-07', 'sa', 'Tabūk', 'Region').
	subdivision('sa-08', 'sa', 'Al Ḩudūd ash Shamālīyah', 'Region').
	subdivision('sa-09', 'sa', 'Jāzān', 'Region').
	subdivision('sa-10', 'sa', 'Najrān', 'Region').
	subdivision('sa-11', 'sa', 'Al Bāḩah', 'Region').
	subdivision('sa-12', 'sa', 'Al Jawf', 'Region').
	subdivision('sa-14', 'sa', '''Asīr', 'Region').
	subdivision('sb-ce', 'sb', 'Central', 'Province').
	subdivision('sb-ch', 'sb', 'Choiseul', 'Province').
	subdivision('sb-ct', 'sb', 'Capital Territory (Honiara)', 'Capital territory').
	subdivision('sb-gu', 'sb', 'Guadalcanal', 'Province').
	subdivision('sb-is', 'sb', 'Isabel', 'Province').
	subdivision('sb-mk', 'sb', 'Makira-Ulawa', 'Province').
	subdivision('sb-ml', 'sb', 'Malaita', 'Province').
	subdivision('sb-rb', 'sb', 'Rennell and Bellona', 'Province').
	subdivision('sb-te', 'sb', 'Temotu', 'Province').
	subdivision('sb-we', 'sb', 'Western', 'Province').
	subdivision('sc-01', 'sc', 'Anse aux Pins', 'District').
	subdivision('sc-02', 'sc', 'Anse Boileau', 'District').
	subdivision('sc-03', 'sc', 'Anse Etoile', 'District').
	subdivision('sc-04', 'sc', 'Au Cap', 'District').
	subdivision('sc-05', 'sc', 'Anse Royale', 'District').
	subdivision('sc-06', 'sc', 'Baie Lazare', 'District').
	subdivision('sc-07', 'sc', 'Baie Sainte Anne', 'District').
	subdivision('sc-08', 'sc', 'Beau Vallon', 'District').
	subdivision('sc-09', 'sc', 'Bel Air', 'District').
	subdivision('sc-10', 'sc', 'Bel Ombre', 'District').
	subdivision('sc-11', 'sc', 'Cascade', 'District').
	subdivision('sc-12', 'sc', 'Glacis', 'District').
	subdivision('sc-13', 'sc', 'Grand Anse Mahe', 'District').
	subdivision('sc-14', 'sc', 'Grand Anse Praslin', 'District').
	subdivision('sc-15', 'sc', 'La Digue', 'District').
	subdivision('sc-16', 'sc', 'English River', 'District').
	subdivision('sc-17', 'sc', 'Mont Buxton', 'District').
	subdivision('sc-18', 'sc', 'Mont Fleuri', 'District').
	subdivision('sc-19', 'sc', 'Plaisance', 'District').
	subdivision('sc-20', 'sc', 'Pointe Larue', 'District').
	subdivision('sc-21', 'sc', 'Port Glaud', 'District').
	subdivision('sc-22', 'sc', 'Saint Louis', 'District').
	subdivision('sc-23', 'sc', 'Takamaka', 'District').
	subdivision('sc-24', 'sc', 'Les Mamelles', 'District').
	subdivision('sc-25', 'sc', 'Roche Caiman', 'District').
	subdivision('sc-26', 'sc', 'Ile Perseverance I', 'District').
	subdivision('sc-27', 'sc', 'Ile Perseverance II', 'District').
	subdivision('sd-dc', 'sd', 'Central Darfur', 'State').
	subdivision('sd-de', 'sd', 'East Darfur', 'State').
	subdivision('sd-dn', 'sd', 'North Darfur', 'State').
	subdivision('sd-ds', 'sd', 'South Darfur', 'State').
	subdivision('sd-dw', 'sd', 'West Darfur', 'State').
	subdivision('sd-gd', 'sd', 'Gedaref', 'State').
	subdivision('sd-gk', 'sd', 'West Kordofan', 'State').
	subdivision('sd-gz', 'sd', 'Gezira', 'State').
	subdivision('sd-ka', 'sd', 'Kassala', 'State').
	subdivision('sd-kh', 'sd', 'Khartoum', 'State').
	subdivision('sd-kn', 'sd', 'North Kordofan', 'State').
	subdivision('sd-ks', 'sd', 'South Kordofan', 'State').
	subdivision('sd-nb', 'sd', 'Blue Nile', 'State').
	subdivision('sd-no', 'sd', 'Northern', 'State').
	subdivision('sd-nr', 'sd', 'River Nile', 'State').
	subdivision('sd-nw', 'sd', 'White Nile', 'State').
	subdivision('sd-rs', 'sd', 'Red Sea', 'State').
	subdivision('sd-si', 'sd', 'Sennar', 'State').
	subdivision('se-ab', 'se', 'Stockholms län [SE-01]', 'County').
	subdivision('se-ac', 'se', 'Västerbottens län [SE-24]', 'County').
	subdivision('se-bd', 'se', 'Norrbottens län [SE-25]', 'County').
	subdivision('se-c', 'se', 'Uppsala län [SE-03]', 'County').
	subdivision('se-d', 'se', 'Södermanlands län [SE-04]', 'County').
	subdivision('se-e', 'se', 'Östergötlands län [SE-05]', 'County').
	subdivision('se-f', 'se', 'Jönköpings län [SE-06]', 'County').
	subdivision('se-g', 'se', 'Kronobergs län [SE-07]', 'County').
	subdivision('se-h', 'se', 'Kalmar län [SE-08]', 'County').
	subdivision('se-i', 'se', 'Gotlands län [SE-09]', 'County').
	subdivision('se-k', 'se', 'Blekinge län [SE-10]', 'County').
	subdivision('se-m', 'se', 'Skåne län [SE-12]', 'County').
	subdivision('se-n', 'se', 'Hallands län [SE-13]', 'County').
	subdivision('se-o', 'se', 'Västra Götalands län [SE-14]', 'County').
	subdivision('se-s', 'se', 'Värmlands län [SE-17]', 'County').
	subdivision('se-t', 'se', 'Örebro län [SE-18]', 'County').
	subdivision('se-u', 'se', 'Västmanlands län [SE-19]', 'County').
	subdivision('se-w', 'se', 'Dalarnas län [SE-20]', 'County').
	subdivision('se-x', 'se', 'Gävleborgs län [SE-21]', 'County').
	subdivision('se-y', 'se', 'Västernorrlands län [SE-22]', 'County').
	subdivision('se-z', 'se', 'Jämtlands län [SE-23]', 'County').
	subdivision('sg-01', 'sg', 'Central Singapore', 'District').
	subdivision('sg-02', 'sg', 'North East', 'District').
	subdivision('sg-03', 'sg', 'North West', 'District').
	subdivision('sg-04', 'sg', 'South East', 'District').
	subdivision('sg-05', 'sg', 'South West', 'District').
	subdivision('sh-ac', 'sh', 'Ascension', 'Geographical entity').
	subdivision('sh-hl', 'sh', 'Saint Helena', 'Geographical entity').
	subdivision('sh-ta', 'sh', 'Tristan da Cunha', 'Geographical entity').
	subdivision('si-001', 'si', 'Ajdovščina', 'Municipality').
	subdivision('si-002', 'si', 'Beltinci', 'Municipality').
	subdivision('si-003', 'si', 'Bled', 'Municipality').
	subdivision('si-004', 'si', 'Bohinj', 'Municipality').
	subdivision('si-005', 'si', 'Borovnica', 'Municipality').
	subdivision('si-006', 'si', 'Bovec', 'Municipality').
	subdivision('si-007', 'si', 'Brda', 'Municipality').
	subdivision('si-008', 'si', 'Brezovica', 'Municipality').
	subdivision('si-009', 'si', 'Brežice', 'Municipality').
	subdivision('si-010', 'si', 'Tišina', 'Municipality').
	subdivision('si-011', 'si', 'Celje', 'Urban municipality').
	subdivision('si-012', 'si', 'Cerklje na Gorenjskem', 'Municipality').
	subdivision('si-013', 'si', 'Cerknica', 'Municipality').
	subdivision('si-014', 'si', 'Cerkno', 'Municipality').
	subdivision('si-015', 'si', 'Črenšovci', 'Municipality').
	subdivision('si-016', 'si', 'Črna na Koroškem', 'Municipality').
	subdivision('si-017', 'si', 'Črnomelj', 'Municipality').
	subdivision('si-018', 'si', 'Destrnik', 'Municipality').
	subdivision('si-019', 'si', 'Divača', 'Municipality').
	subdivision('si-020', 'si', 'Dobrepolje', 'Municipality').
	subdivision('si-021', 'si', 'Dobrova-Polhov Gradec', 'Municipality').
	subdivision('si-022', 'si', 'Dol pri Ljubljani', 'Municipality').
	subdivision('si-023', 'si', 'Domžale', 'Municipality').
	subdivision('si-024', 'si', 'Dornava', 'Municipality').
	subdivision('si-025', 'si', 'Dravograd', 'Municipality').
	subdivision('si-026', 'si', 'Duplek', 'Municipality').
	subdivision('si-027', 'si', 'Gorenja vas-Poljane', 'Municipality').
	subdivision('si-028', 'si', 'Gorišnica', 'Municipality').
	subdivision('si-029', 'si', 'Gornja Radgona', 'Municipality').
	subdivision('si-030', 'si', 'Gornji Grad', 'Municipality').
	subdivision('si-031', 'si', 'Gornji Petrovci', 'Municipality').
	subdivision('si-032', 'si', 'Grosuplje', 'Municipality').
	subdivision('si-033', 'si', 'Šalovci', 'Municipality').
	subdivision('si-034', 'si', 'Hrastnik', 'Municipality').
	subdivision('si-035', 'si', 'Hrpelje-Kozina', 'Municipality').
	subdivision('si-036', 'si', 'Idrija', 'Municipality').
	subdivision('si-037', 'si', 'Ig', 'Municipality').
	subdivision('si-038', 'si', 'Ilirska Bistrica', 'Municipality').
	subdivision('si-039', 'si', 'Ivančna Gorica', 'Municipality').
	subdivision('si-040', 'si', 'Izola', 'Municipality').
	subdivision('si-041', 'si', 'Jesenice', 'Municipality').
	subdivision('si-042', 'si', 'Juršinci', 'Municipality').
	subdivision('si-043', 'si', 'Kamnik', 'Municipality').
	subdivision('si-044', 'si', 'Kanal ob Soči', 'Municipality').
	subdivision('si-045', 'si', 'Kidričevo', 'Municipality').
	subdivision('si-046', 'si', 'Kobarid', 'Municipality').
	subdivision('si-047', 'si', 'Kobilje', 'Municipality').
	subdivision('si-048', 'si', 'Kočevje', 'Municipality').
	subdivision('si-049', 'si', 'Komen', 'Municipality').
	subdivision('si-050', 'si', 'Koper', 'Urban municipality').
	subdivision('si-051', 'si', 'Kozje', 'Municipality').
	subdivision('si-052', 'si', 'Kranj', 'Urban municipality').
	subdivision('si-053', 'si', 'Kranjska Gora', 'Municipality').
	subdivision('si-054', 'si', 'Krško', 'Urban municipality').
	subdivision('si-055', 'si', 'Kungota', 'Municipality').
	subdivision('si-056', 'si', 'Kuzma', 'Municipality').
	subdivision('si-057', 'si', 'Laško', 'Municipality').
	subdivision('si-058', 'si', 'Lenart', 'Municipality').
	subdivision('si-059', 'si', 'Lendava', 'Municipality').
	subdivision('si-060', 'si', 'Litija', 'Municipality').
	subdivision('si-061', 'si', 'Ljubljana', 'Urban municipality').
	subdivision('si-062', 'si', 'Ljubno', 'Municipality').
	subdivision('si-063', 'si', 'Ljutomer', 'Municipality').
	subdivision('si-064', 'si', 'Logatec', 'Municipality').
	subdivision('si-065', 'si', 'Loška dolina', 'Municipality').
	subdivision('si-066', 'si', 'Loški Potok', 'Municipality').
	subdivision('si-067', 'si', 'Luče', 'Municipality').
	subdivision('si-068', 'si', 'Lukovica', 'Municipality').
	subdivision('si-069', 'si', 'Majšperk', 'Municipality').
	subdivision('si-070', 'si', 'Maribor', 'Urban municipality').
	subdivision('si-071', 'si', 'Medvode', 'Municipality').
	subdivision('si-072', 'si', 'Mengeš', 'Municipality').
	subdivision('si-073', 'si', 'Metlika', 'Municipality').
	subdivision('si-074', 'si', 'Mežica', 'Municipality').
	subdivision('si-075', 'si', 'Miren-Kostanjevica', 'Municipality').
	subdivision('si-076', 'si', 'Mislinja', 'Municipality').
	subdivision('si-077', 'si', 'Moravče', 'Municipality').
	subdivision('si-078', 'si', 'Moravske Toplice', 'Municipality').
	subdivision('si-079', 'si', 'Mozirje', 'Municipality').
	subdivision('si-080', 'si', 'Murska Sobota', 'Urban municipality').
	subdivision('si-081', 'si', 'Muta', 'Municipality').
	subdivision('si-082', 'si', 'Naklo', 'Municipality').
	subdivision('si-083', 'si', 'Nazarje', 'Municipality').
	subdivision('si-084', 'si', 'Nova Gorica', 'Urban municipality').
	subdivision('si-085', 'si', 'Novo Mesto', 'Urban municipality').
	subdivision('si-086', 'si', 'Odranci', 'Municipality').
	subdivision('si-087', 'si', 'Ormož', 'Municipality').
	subdivision('si-088', 'si', 'Osilnica', 'Municipality').
	subdivision('si-089', 'si', 'Pesnica', 'Municipality').
	subdivision('si-090', 'si', 'Piran', 'Municipality').
	subdivision('si-091', 'si', 'Pivka', 'Municipality').
	subdivision('si-092', 'si', 'Podčetrtek', 'Municipality').
	subdivision('si-093', 'si', 'Podvelka', 'Municipality').
	subdivision('si-094', 'si', 'Postojna', 'Municipality').
	subdivision('si-095', 'si', 'Preddvor', 'Municipality').
	subdivision('si-096', 'si', 'Ptuj', 'Urban municipality').
	subdivision('si-097', 'si', 'Puconci', 'Municipality').
	subdivision('si-098', 'si', 'Rače-Fram', 'Municipality').
	subdivision('si-099', 'si', 'Radeče', 'Municipality').
	subdivision('si-100', 'si', 'Radenci', 'Municipality').
	subdivision('si-101', 'si', 'Radlje ob Dravi', 'Municipality').
	subdivision('si-102', 'si', 'Radovljica', 'Municipality').
	subdivision('si-103', 'si', 'Ravne na Koroškem', 'Municipality').
	subdivision('si-104', 'si', 'Ribnica', 'Municipality').
	subdivision('si-105', 'si', 'Rogašovci', 'Municipality').
	subdivision('si-106', 'si', 'Rogaška Slatina', 'Municipality').
	subdivision('si-107', 'si', 'Rogatec', 'Municipality').
	subdivision('si-108', 'si', 'Ruše', 'Municipality').
	subdivision('si-109', 'si', 'Semič', 'Municipality').
	subdivision('si-110', 'si', 'Sevnica', 'Municipality').
	subdivision('si-111', 'si', 'Sežana', 'Municipality').
	subdivision('si-112', 'si', 'Slovenj Gradec', 'Urban municipality').
	subdivision('si-113', 'si', 'Slovenska Bistrica', 'Municipality').
	subdivision('si-114', 'si', 'Slovenske Konjice', 'Municipality').
	subdivision('si-115', 'si', 'Starše', 'Municipality').
	subdivision('si-116', 'si', 'Sveti Jurij ob Ščavnici', 'Municipality').
	subdivision('si-117', 'si', 'Šenčur', 'Municipality').
	subdivision('si-118', 'si', 'Šentilj', 'Municipality').
	subdivision('si-119', 'si', 'Šentjernej', 'Municipality').
	subdivision('si-120', 'si', 'Šentjur', 'Municipality').
	subdivision('si-121', 'si', 'Škocjan', 'Municipality').
	subdivision('si-122', 'si', 'Škofja Loka', 'Municipality').
	subdivision('si-123', 'si', 'Škofljica', 'Municipality').
	subdivision('si-124', 'si', 'Šmarje pri Jelšah', 'Municipality').
	subdivision('si-125', 'si', 'Šmartno ob Paki', 'Municipality').
	subdivision('si-126', 'si', 'Šoštanj', 'Municipality').
	subdivision('si-127', 'si', 'Štore', 'Municipality').
	subdivision('si-128', 'si', 'Tolmin', 'Municipality').
	subdivision('si-129', 'si', 'Trbovlje', 'Municipality').
	subdivision('si-130', 'si', 'Trebnje', 'Municipality').
	subdivision('si-131', 'si', 'Tržič', 'Municipality').
	subdivision('si-132', 'si', 'Turnišče', 'Municipality').
	subdivision('si-133', 'si', 'Velenje', 'Urban municipality').
	subdivision('si-134', 'si', 'Velike Lašče', 'Municipality').
	subdivision('si-135', 'si', 'Videm', 'Municipality').
	subdivision('si-136', 'si', 'Vipava', 'Municipality').
	subdivision('si-137', 'si', 'Vitanje', 'Municipality').
	subdivision('si-138', 'si', 'Vodice', 'Municipality').
	subdivision('si-139', 'si', 'Vojnik', 'Municipality').
	subdivision('si-140', 'si', 'Vrhnika', 'Municipality').
	subdivision('si-141', 'si', 'Vuzenica', 'Municipality').
	subdivision('si-142', 'si', 'Zagorje ob Savi', 'Municipality').
	subdivision('si-143', 'si', 'Zavrč', 'Municipality').
	subdivision('si-144', 'si', 'Zreče', 'Municipality').
	subdivision('si-146', 'si', 'Železniki', 'Municipality').
	subdivision('si-147', 'si', 'Žiri', 'Municipality').
	subdivision('si-148', 'si', 'Benedikt', 'Municipality').
	subdivision('si-149', 'si', 'Bistrica ob Sotli', 'Municipality').
	subdivision('si-150', 'si', 'Bloke', 'Municipality').
	subdivision('si-151', 'si', 'Braslovče', 'Municipality').
	subdivision('si-152', 'si', 'Cankova', 'Municipality').
	subdivision('si-153', 'si', 'Cerkvenjak', 'Municipality').
	subdivision('si-154', 'si', 'Dobje', 'Municipality').
	subdivision('si-155', 'si', 'Dobrna', 'Municipality').
	subdivision('si-156', 'si', 'Dobrovnik', 'Municipality').
	subdivision('si-157', 'si', 'Dolenjske Toplice', 'Municipality').
	subdivision('si-158', 'si', 'Grad', 'Municipality').
	subdivision('si-159', 'si', 'Hajdina', 'Municipality').
	subdivision('si-160', 'si', 'Hoče-Slivnica', 'Municipality').
	subdivision('si-161', 'si', 'Hodoš', 'Municipality').
	subdivision('si-162', 'si', 'Horjul', 'Municipality').
	subdivision('si-163', 'si', 'Jezersko', 'Municipality').
	subdivision('si-164', 'si', 'Komenda', 'Municipality').
	subdivision('si-165', 'si', 'Kostel', 'Municipality').
	subdivision('si-166', 'si', 'Križevci', 'Municipality').
	subdivision('si-167', 'si', 'Lovrenc na Pohorju', 'Municipality').
	subdivision('si-168', 'si', 'Markovci', 'Municipality').
	subdivision('si-169', 'si', 'Miklavž na Dravskem polju', 'Municipality').
	subdivision('si-170', 'si', 'Mirna Peč', 'Municipality').
	subdivision('si-171', 'si', 'Oplotnica', 'Municipality').
	subdivision('si-172', 'si', 'Podlehnik', 'Municipality').
	subdivision('si-173', 'si', 'Polzela', 'Municipality').
	subdivision('si-174', 'si', 'Prebold', 'Municipality').
	subdivision('si-175', 'si', 'Prevalje', 'Municipality').
	subdivision('si-176', 'si', 'Razkrižje', 'Municipality').
	subdivision('si-177', 'si', 'Ribnica na Pohorju', 'Municipality').
	subdivision('si-178', 'si', 'Selnica ob Dravi', 'Municipality').
	subdivision('si-179', 'si', 'Sodražica', 'Municipality').
	subdivision('si-180', 'si', 'Solčava', 'Municipality').
	subdivision('si-181', 'si', 'Sveta Ana', 'Municipality').
	subdivision('si-182', 'si', 'Sveti Andraž v Slovenskih goricah', 'Municipality').
	subdivision('si-183', 'si', 'Šempeter-Vrtojba', 'Municipality').
	subdivision('si-184', 'si', 'Tabor', 'Municipality').
	subdivision('si-185', 'si', 'Trnovska Vas', 'Municipality').
	subdivision('si-186', 'si', 'Trzin', 'Municipality').
	subdivision('si-187', 'si', 'Velika Polana', 'Municipality').
	subdivision('si-188', 'si', 'Veržej', 'Municipality').
	subdivision('si-189', 'si', 'Vransko', 'Municipality').
	subdivision('si-190', 'si', 'Žalec', 'Municipality').
	subdivision('si-191', 'si', 'Žetale', 'Municipality').
	subdivision('si-192', 'si', 'Žirovnica', 'Municipality').
	subdivision('si-193', 'si', 'Žužemberk', 'Municipality').
	subdivision('si-194', 'si', 'Šmartno pri Litiji', 'Municipality').
	subdivision('si-195', 'si', 'Apače', 'Municipality').
	subdivision('si-196', 'si', 'Cirkulane', 'Municipality').
	subdivision('si-197', 'si', 'Kostanjevica na Krki', 'Municipality').
	subdivision('si-198', 'si', 'Makole', 'Municipality').
	subdivision('si-199', 'si', 'Mokronog-Trebelno', 'Municipality').
	subdivision('si-200', 'si', 'Poljčane', 'Municipality').
	subdivision('si-201', 'si', 'Renče-Vogrsko', 'Municipality').
	subdivision('si-202', 'si', 'Središče ob Dravi', 'Municipality').
	subdivision('si-203', 'si', 'Straža', 'Municipality').
	subdivision('si-204', 'si', 'Sveta Trojica v Slovenskih goricah', 'Municipality').
	subdivision('si-205', 'si', 'Sveti Tomaž', 'Municipality').
	subdivision('si-206', 'si', 'Šmarješke Toplice', 'Municipality').
	subdivision('si-207', 'si', 'Gorje', 'Municipality').
	subdivision('si-208', 'si', 'Log-Dragomer', 'Municipality').
	subdivision('si-209', 'si', 'Rečica ob Savinji', 'Municipality').
	subdivision('si-210', 'si', 'Sveti Jurij v Slovenskih goricah', 'Municipality').
	subdivision('si-211', 'si', 'Šentrupert', 'Municipality').
	subdivision('si-212', 'si', 'Mirna', 'Municipality').
	subdivision('si-213', 'si', 'Ankaran', 'Municipality').
	subdivision('sk-bc', 'sk', 'Banskobystrický kraj', 'Region').
	subdivision('sk-bl', 'sk', 'Bratislavský kraj', 'Region').
	subdivision('sk-ki', 'sk', 'Košický kraj', 'Region').
	subdivision('sk-ni', 'sk', 'Nitriansky kraj', 'Region').
	subdivision('sk-pv', 'sk', 'Prešovský kraj', 'Region').
	subdivision('sk-ta', 'sk', 'Trnavský kraj', 'Region').
	subdivision('sk-tc', 'sk', 'Trenčiansky kraj', 'Region').
	subdivision('sk-zi', 'sk', 'Žilinský kraj', 'Region').
	subdivision('sl-e', 'sl', 'Eastern', 'Province').
	subdivision('sl-n', 'sl', 'Northern', 'Province').
	subdivision('sl-nw', 'sl', 'North Western', 'Province').
	subdivision('sl-s', 'sl', 'Southern', 'Province').
	subdivision('sl-w', 'sl', 'Western Area (Freetown)', 'Area').
	subdivision('sm-01', 'sm', 'Acquaviva', 'Municipality').
	subdivision('sm-02', 'sm', 'Chiesanuova', 'Municipality').
	subdivision('sm-03', 'sm', 'Domagnano', 'Municipality').
	subdivision('sm-04', 'sm', 'Faetano', 'Municipality').
	subdivision('sm-05', 'sm', 'Fiorentino', 'Municipality').
	subdivision('sm-06', 'sm', 'Borgo Maggiore', 'Municipality').
	subdivision('sm-07', 'sm', 'Città di San Marino', 'Municipality').
	subdivision('sm-08', 'sm', 'Montegiardino', 'Municipality').
	subdivision('sm-09', 'sm', 'Serravalle', 'Municipality').
	subdivision('sn-db', 'sn', 'Diourbel', 'Region').
	subdivision('sn-dk', 'sn', 'Dakar', 'Region').
	subdivision('sn-fk', 'sn', 'Fatick', 'Region').
	subdivision('sn-ka', 'sn', 'Kaffrine', 'Region').
	subdivision('sn-kd', 'sn', 'Kolda', 'Region').
	subdivision('sn-ke', 'sn', 'Kédougou', 'Region').
	subdivision('sn-kl', 'sn', 'Kaolack', 'Region').
	subdivision('sn-lg', 'sn', 'Louga', 'Region').
	subdivision('sn-mt', 'sn', 'Matam', 'Region').
	subdivision('sn-se', 'sn', 'Sédhiou', 'Region').
	subdivision('sn-sl', 'sn', 'Saint-Louis', 'Region').
	subdivision('sn-tc', 'sn', 'Tambacounda', 'Region').
	subdivision('sn-th', 'sn', 'Thiès', 'Region').
	subdivision('sn-zg', 'sn', 'Ziguinchor', 'Region').
	subdivision('so-aw', 'so', 'Awdal', 'Region').
	subdivision('so-bk', 'so', 'Bakool', 'Region').
	subdivision('so-bn', 'so', 'Banaadir', 'Region').
	subdivision('so-br', 'so', 'Bari', 'Region').
	subdivision('so-by', 'so', 'Bay', 'Region').
	subdivision('so-ga', 'so', 'Galguduud', 'Region').
	subdivision('so-ge', 'so', 'Gedo', 'Region').
	subdivision('so-hi', 'so', 'Hiiraan', 'Region').
	subdivision('so-jd', 'so', 'Jubbada Dhexe', 'Region').
	subdivision('so-jh', 'so', 'Jubbada Hoose', 'Region').
	subdivision('so-mu', 'so', 'Mudug', 'Region').
	subdivision('so-nu', 'so', 'Nugaal', 'Region').
	subdivision('so-sa', 'so', 'Sanaag', 'Region').
	subdivision('so-sd', 'so', 'Shabeellaha Dhexe', 'Region').
	subdivision('so-sh', 'so', 'Shabeellaha Hoose', 'Region').
	subdivision('so-so', 'so', 'Sool', 'Region').
	subdivision('so-to', 'so', 'Togdheer', 'Region').
	subdivision('so-wo', 'so', 'Woqooyi Galbeed', 'Region').
	subdivision('sr-br', 'sr', 'Brokopondo', 'District').
	subdivision('sr-cm', 'sr', 'Commewijne', 'District').
	subdivision('sr-cr', 'sr', 'Coronie', 'District').
	subdivision('sr-ma', 'sr', 'Marowijne', 'District').
	subdivision('sr-ni', 'sr', 'Nickerie', 'District').
	subdivision('sr-pm', 'sr', 'Paramaribo', 'District').
	subdivision('sr-pr', 'sr', 'Para', 'District').
	subdivision('sr-sa', 'sr', 'Saramacca', 'District').
	subdivision('sr-si', 'sr', 'Sipaliwini', 'District').
	subdivision('sr-wa', 'sr', 'Wanica', 'District').
	subdivision('ss-bn', 'ss', 'Northern Bahr el Ghazal', 'State').
	subdivision('ss-bw', 'ss', 'Western Bahr el Ghazal', 'State').
	subdivision('ss-ec', 'ss', 'Central Equatoria', 'State').
	subdivision('ss-ee', 'ss', 'Eastern Equatoria', 'State').
	subdivision('ss-ew', 'ss', 'Western Equatoria', 'State').
	subdivision('ss-jg', 'ss', 'Jonglei', 'State').
	subdivision('ss-lk', 'ss', 'Lakes', 'State').
	subdivision('ss-nu', 'ss', 'Upper Nile', 'State').
	subdivision('ss-uy', 'ss', 'Unity', 'State').
	subdivision('ss-wr', 'ss', 'Warrap', 'State').
	subdivision('st-01', 'st', 'Água Grande', 'District').
	subdivision('st-02', 'st', 'Cantagalo', 'District').
	subdivision('st-03', 'st', 'Caué', 'District').
	subdivision('st-04', 'st', 'Lembá', 'District').
	subdivision('st-05', 'st', 'Lobata', 'District').
	subdivision('st-06', 'st', 'Mé-Zóchi', 'District').
	subdivision('st-p', 'st', 'Príncipe', 'Autonomous region').
	subdivision('sv-ah', 'sv', 'Ahuachapán', 'Department').
	subdivision('sv-ca', 'sv', 'Cabañas', 'Department').
	subdivision('sv-ch', 'sv', 'Chalatenango', 'Department').
	subdivision('sv-cu', 'sv', 'Cuscatlán', 'Department').
	subdivision('sv-li', 'sv', 'La Libertad', 'Department').
	subdivision('sv-mo', 'sv', 'Morazán', 'Department').
	subdivision('sv-pa', 'sv', 'La Paz', 'Department').
	subdivision('sv-sa', 'sv', 'Santa Ana', 'Department').
	subdivision('sv-sm', 'sv', 'San Miguel', 'Department').
	subdivision('sv-so', 'sv', 'Sonsonate', 'Department').
	subdivision('sv-ss', 'sv', 'San Salvador', 'Department').
	subdivision('sv-sv', 'sv', 'San Vicente', 'Department').
	subdivision('sv-un', 'sv', 'La Unión', 'Department').
	subdivision('sv-us', 'sv', 'Usulután', 'Department').
	subdivision('sy-di', 'sy', 'Dimashq', 'Province').
	subdivision('sy-dr', 'sy', 'Dar''ā', 'Province').
	subdivision('sy-dy', 'sy', 'Dayr az Zawr', 'Province').
	subdivision('sy-ha', 'sy', 'Al Ḩasakah', 'Province').
	subdivision('sy-hi', 'sy', 'Ḩimş', 'Province').
	subdivision('sy-hl', 'sy', 'Ḩalab', 'Province').
	subdivision('sy-hm', 'sy', 'Ḩamāh', 'Province').
	subdivision('sy-id', 'sy', 'Idlib', 'Province').
	subdivision('sy-la', 'sy', 'Al Lādhiqīyah', 'Province').
	subdivision('sy-qu', 'sy', 'Al Qunayţirah', 'Province').
	subdivision('sy-ra', 'sy', 'Ar Raqqah', 'Province').
	subdivision('sy-rd', 'sy', 'Rīf Dimashq', 'Province').
	subdivision('sy-su', 'sy', 'As Suwaydā''', 'Province').
	subdivision('sy-ta', 'sy', 'Ţarţūs', 'Province').
	subdivision('sz-hh', 'sz', 'Hhohho', 'Region').
	subdivision('sz-lu', 'sz', 'Lubombo', 'Region').
	subdivision('sz-ma', 'sz', 'Manzini', 'Region').
	subdivision('sz-sh', 'sz', 'Shiselweni', 'Region').
	subdivision('td-ba', 'td', 'Batha', 'Province').
	subdivision('td-bg', 'td', 'Bahr el Ghazal', 'Province').
	subdivision('td-bo', 'td', 'Borkou', 'Province').
	subdivision('td-cb', 'td', 'Chari-Baguirmi', 'Province').
	subdivision('td-ee', 'td', 'Ennedi-Est', 'Province').
	subdivision('td-eo', 'td', 'Ennedi-Ouest', 'Province').
	subdivision('td-gr', 'td', 'Guéra', 'Province').
	subdivision('td-hl', 'td', 'Hadjer Lamis', 'Province').
	subdivision('td-ka', 'td', 'Kanem', 'Province').
	subdivision('td-lc', 'td', 'Lac', 'Province').
	subdivision('td-lo', 'td', 'Logone-Occidental', 'Province').
	subdivision('td-lr', 'td', 'Logone-Oriental', 'Province').
	subdivision('td-ma', 'td', 'Mandoul', 'Province').
	subdivision('td-mc', 'td', 'Moyen-Chari', 'Province').
	subdivision('td-me', 'td', 'Mayo-Kebbi-Est', 'Province').
	subdivision('td-mo', 'td', 'Mayo-Kebbi-Ouest', 'Province').
	subdivision('td-nd', 'td', 'Ville de Ndjamena', 'Province').
	subdivision('td-od', 'td', 'Ouaddaï', 'Province').
	subdivision('td-sa', 'td', 'Salamat', 'Province').
	subdivision('td-si', 'td', 'Sila', 'Province').
	subdivision('td-ta', 'td', 'Tandjilé', 'Province').
	subdivision('td-ti', 'td', 'Tibesti', 'Province').
	subdivision('td-wf', 'td', 'Wadi Fira', 'Province').
	subdivision('tg-c', 'tg', 'Centrale', 'Region').
	subdivision('tg-k', 'tg', 'Kara', 'Region').
	subdivision('tg-m', 'tg', 'Maritime (Région)', 'Region').
	subdivision('tg-p', 'tg', 'Plateaux', 'Region').
	subdivision('tg-s', 'tg', 'Savanes', 'Region').
	subdivision('th-10', 'th', 'Krung Thep Maha Nakhon', 'Metropolitan administration').
	subdivision('th-11', 'th', 'Samut Prakan', 'Province').
	subdivision('th-12', 'th', 'Nonthaburi', 'Province').
	subdivision('th-13', 'th', 'Pathum Thani', 'Province').
	subdivision('th-14', 'th', 'Phra Nakhon Si Ayutthaya', 'Province').
	subdivision('th-15', 'th', 'Ang Thong', 'Province').
	subdivision('th-16', 'th', 'Lop Buri', 'Province').
	subdivision('th-17', 'th', 'Sing Buri', 'Province').
	subdivision('th-18', 'th', 'Chai Nat', 'Province').
	subdivision('th-19', 'th', 'Saraburi', 'Province').
	subdivision('th-20', 'th', 'Chon Buri', 'Province').
	subdivision('th-21', 'th', 'Rayong', 'Province').
	subdivision('th-22', 'th', 'Chanthaburi', 'Province').
	subdivision('th-23', 'th', 'Trat', 'Province').
	subdivision('th-24', 'th', 'Chachoengsao', 'Province').
	subdivision('th-25', 'th', 'Prachin Buri', 'Province').
	subdivision('th-26', 'th', 'Nakhon Nayok', 'Province').
	subdivision('th-27', 'th', 'Sa Kaeo', 'Province').
	subdivision('th-30', 'th', 'Nakhon Ratchasima', 'Province').
	subdivision('th-31', 'th', 'Buri Ram', 'Province').
	subdivision('th-32', 'th', 'Surin', 'Province').
	subdivision('th-33', 'th', 'Si Sa Ket', 'Province').
	subdivision('th-34', 'th', 'Ubon Ratchathani', 'Province').
	subdivision('th-35', 'th', 'Yasothon', 'Province').
	subdivision('th-36', 'th', 'Chaiyaphum', 'Province').
	subdivision('th-37', 'th', 'Amnat Charoen', 'Province').
	subdivision('th-38', 'th', 'Bueng Kan', 'Province').
	subdivision('th-39', 'th', 'Nong Bua Lam Phu', 'Province').
	subdivision('th-40', 'th', 'Khon Kaen', 'Province').
	subdivision('th-41', 'th', 'Udon Thani', 'Province').
	subdivision('th-42', 'th', 'Loei', 'Province').
	subdivision('th-43', 'th', 'Nong Khai', 'Province').
	subdivision('th-44', 'th', 'Maha Sarakham', 'Province').
	subdivision('th-45', 'th', 'Roi Et', 'Province').
	subdivision('th-46', 'th', 'Kalasin', 'Province').
	subdivision('th-47', 'th', 'Sakon Nakhon', 'Province').
	subdivision('th-48', 'th', 'Nakhon Phanom', 'Province').
	subdivision('th-49', 'th', 'Mukdahan', 'Province').
	subdivision('th-50', 'th', 'Chiang Mai', 'Province').
	subdivision('th-51', 'th', 'Lamphun', 'Province').
	subdivision('th-52', 'th', 'Lampang', 'Province').
	subdivision('th-53', 'th', 'Uttaradit', 'Province').
	subdivision('th-54', 'th', 'Phrae', 'Province').
	subdivision('th-55', 'th', 'Nan', 'Province').
	subdivision('th-56', 'th', 'Phayao', 'Province').
	subdivision('th-57', 'th', 'Chiang Rai', 'Province').
	subdivision('th-58', 'th', 'Mae Hong Son', 'Province').
	subdivision('th-60', 'th', 'Nakhon Sawan', 'Province').
	subdivision('th-61', 'th', 'Uthai Thani', 'Province').
	subdivision('th-62', 'th', 'Kamphaeng Phet', 'Province').
	subdivision('th-63', 'th', 'Tak', 'Province').
	subdivision('th-64', 'th', 'Sukhothai', 'Province').
	subdivision('th-65', 'th', 'Phitsanulok', 'Province').
	subdivision('th-66', 'th', 'Phichit', 'Province').
	subdivision('th-67', 'th', 'Phetchabun', 'Province').
	subdivision('th-70', 'th', 'Ratchaburi', 'Province').
	subdivision('th-71', 'th', 'Kanchanaburi', 'Province').
	subdivision('th-72', 'th', 'Suphan Buri', 'Province').
	subdivision('th-73', 'th', 'Nakhon Pathom', 'Province').
	subdivision('th-74', 'th', 'Samut Sakhon', 'Province').
	subdivision('th-75', 'th', 'Samut Songkhram', 'Province').
	subdivision('th-76', 'th', 'Phetchaburi', 'Province').
	subdivision('th-77', 'th', 'Prachuap Khiri Khan', 'Province').
	subdivision('th-80', 'th', 'Nakhon Si Thammarat', 'Province').
	subdivision('th-81', 'th', 'Krabi', 'Province').
	subdivision('th-82', 'th', 'Phangnga', 'Province').
	subdivision('th-83', 'th', 'Phuket', 'Province').
	subdivision('th-84', 'th', 'Surat Thani', 'Province').
	subdivision('th-85', 'th', 'Ranong', 'Province').
	subdivision('th-86', 'th', 'Chumphon', 'Province').
	subdivision('th-90', 'th', 'Songkhla', 'Province').
	subdivision('th-91', 'th', 'Satun', 'Province').
	subdivision('th-92', 'th', 'Trang', 'Province').
	subdivision('th-93', 'th', 'Phatthalung', 'Province').
	subdivision('th-94', 'th', 'Pattani', 'Province').
	subdivision('th-95', 'th', 'Yala', 'Province').
	subdivision('th-96', 'th', 'Narathiwat', 'Province').
	subdivision('th-s', 'th', 'Phatthaya', 'Special administrative city').
	subdivision('tj-du', 'tj', 'Dushanbe', 'Capital territory').
	subdivision('tj-gb', 'tj', 'Kŭhistoni Badakhshon', 'Autonomous region').
	subdivision('tj-kt', 'tj', 'Khatlon', 'Region').
	subdivision('tj-ra', 'tj', 'nohiyahoi tobei jumhurí', 'Districts under republic administration').
	subdivision('tj-su', 'tj', 'Sughd', 'Region').
	subdivision('tl-al', 'tl', 'Aileu', 'Municipality').
	subdivision('tl-an', 'tl', 'Ainaro', 'Municipality').
	subdivision('tl-ba', 'tl', 'Baucau', 'Municipality').
	subdivision('tl-bo', 'tl', 'Bobonaro', 'Municipality').
	subdivision('tl-co', 'tl', 'Cova Lima', 'Municipality').
	subdivision('tl-di', 'tl', 'Díli', 'Municipality').
	subdivision('tl-er', 'tl', 'Ermera', 'Municipality').
	subdivision('tl-la', 'tl', 'Lautém', 'Municipality').
	subdivision('tl-li', 'tl', 'Liquiça', 'Municipality').
	subdivision('tl-mf', 'tl', 'Manufahi', 'Municipality').
	subdivision('tl-mt', 'tl', 'Manatuto', 'Municipality').
	subdivision('tl-oe', 'tl', 'Oé-Cusse Ambeno', 'Special administrative region').
	subdivision('tl-vi', 'tl', 'Viqueque', 'Municipality').
	subdivision('tm-a', 'tm', 'Ahal', 'Region').
	subdivision('tm-b', 'tm', 'Balkan', 'Region').
	subdivision('tm-d', 'tm', 'Daşoguz', 'Region').
	subdivision('tm-l', 'tm', 'Lebap', 'Region').
	subdivision('tm-m', 'tm', 'Mary', 'Region').
	subdivision('tm-s', 'tm', 'Aşgabat', 'City').
	subdivision('tn-11', 'tn', 'Tunis', 'Governorate').
	subdivision('tn-12', 'tn', 'L''Ariana', 'Governorate').
	subdivision('tn-13', 'tn', 'Ben Arous', 'Governorate').
	subdivision('tn-14', 'tn', 'La Manouba', 'Governorate').
	subdivision('tn-21', 'tn', 'Nabeul', 'Governorate').
	subdivision('tn-22', 'tn', 'Zaghouan', 'Governorate').
	subdivision('tn-23', 'tn', 'Bizerte', 'Governorate').
	subdivision('tn-31', 'tn', 'Béja', 'Governorate').
	subdivision('tn-32', 'tn', 'Jendouba', 'Governorate').
	subdivision('tn-33', 'tn', 'Le Kef', 'Governorate').
	subdivision('tn-34', 'tn', 'Siliana', 'Governorate').
	subdivision('tn-41', 'tn', 'Kairouan', 'Governorate').
	subdivision('tn-42', 'tn', 'Kasserine', 'Governorate').
	subdivision('tn-43', 'tn', 'Sidi Bouzid', 'Governorate').
	subdivision('tn-51', 'tn', 'Sousse', 'Governorate').
	subdivision('tn-52', 'tn', 'Monastir', 'Governorate').
	subdivision('tn-53', 'tn', 'Mahdia', 'Governorate').
	subdivision('tn-61', 'tn', 'Sfax', 'Governorate').
	subdivision('tn-71', 'tn', 'Gafsa', 'Governorate').
	subdivision('tn-72', 'tn', 'Tozeur', 'Governorate').
	subdivision('tn-73', 'tn', 'Kébili', 'Governorate').
	subdivision('tn-81', 'tn', 'Gabès', 'Governorate').
	subdivision('tn-82', 'tn', 'Médenine', 'Governorate').
	subdivision('tn-83', 'tn', 'Tataouine', 'Governorate').
	subdivision('to-01', 'to', '''Eua', 'Division').
	subdivision('to-02', 'to', 'Ha''apai', 'Division').
	subdivision('to-03', 'to', 'Niuas', 'Division').
	subdivision('to-04', 'to', 'Tongatapu', 'Division').
	subdivision('to-05', 'to', 'Vava''u', 'Division').
	subdivision('tr-01', 'tr', 'Adana', 'Province').
	subdivision('tr-02', 'tr', 'Adıyaman', 'Province').
	subdivision('tr-03', 'tr', 'Afyonkarahisar', 'Province').
	subdivision('tr-04', 'tr', 'Ağrı', 'Province').
	subdivision('tr-05', 'tr', 'Amasya', 'Province').
	subdivision('tr-06', 'tr', 'Ankara', 'Province').
	subdivision('tr-07', 'tr', 'Antalya', 'Province').
	subdivision('tr-08', 'tr', 'Artvin', 'Province').
	subdivision('tr-09', 'tr', 'Aydın', 'Province').
	subdivision('tr-10', 'tr', 'Balıkesir', 'Province').
	subdivision('tr-11', 'tr', 'Bilecik', 'Province').
	subdivision('tr-12', 'tr', 'Bingöl', 'Province').
	subdivision('tr-13', 'tr', 'Bitlis', 'Province').
	subdivision('tr-14', 'tr', 'Bolu', 'Province').
	subdivision('tr-15', 'tr', 'Burdur', 'Province').
	subdivision('tr-16', 'tr', 'Bursa', 'Province').
	subdivision('tr-17', 'tr', 'Çanakkale', 'Province').
	subdivision('tr-18', 'tr', 'Çankırı', 'Province').
	subdivision('tr-19', 'tr', 'Çorum', 'Province').
	subdivision('tr-20', 'tr', 'Denizli', 'Province').
	subdivision('tr-21', 'tr', 'Diyarbakır', 'Province').
	subdivision('tr-22', 'tr', 'Edirne', 'Province').
	subdivision('tr-23', 'tr', 'Elazığ', 'Province').
	subdivision('tr-24', 'tr', 'Erzincan', 'Province').
	subdivision('tr-25', 'tr', 'Erzurum', 'Province').
	subdivision('tr-26', 'tr', 'Eskişehir', 'Province').
	subdivision('tr-27', 'tr', 'Gaziantep', 'Province').
	subdivision('tr-28', 'tr', 'Giresun', 'Province').
	subdivision('tr-29', 'tr', 'Gümüşhane', 'Province').
	subdivision('tr-30', 'tr', 'Hakkâri', 'Province').
	subdivision('tr-31', 'tr', 'Hatay', 'Province').
	subdivision('tr-32', 'tr', 'Isparta', 'Province').
	subdivision('tr-33', 'tr', 'Mersin', 'Province').
	subdivision('tr-34', 'tr', 'İstanbul', 'Province').
	subdivision('tr-35', 'tr', 'İzmir', 'Province').
	subdivision('tr-36', 'tr', 'Kars', 'Province').
	subdivision('tr-37', 'tr', 'Kastamonu', 'Province').
	subdivision('tr-38', 'tr', 'Kayseri', 'Province').
	subdivision('tr-39', 'tr', 'Kırklareli', 'Province').
	subdivision('tr-40', 'tr', 'Kırşehir', 'Province').
	subdivision('tr-41', 'tr', 'Kocaeli', 'Province').
	subdivision('tr-42', 'tr', 'Konya', 'Province').
	subdivision('tr-43', 'tr', 'Kütahya', 'Province').
	subdivision('tr-44', 'tr', 'Malatya', 'Province').
	subdivision('tr-45', 'tr', 'Manisa', 'Province').
	subdivision('tr-46', 'tr', 'Kahramanmaraş', 'Province').
	subdivision('tr-47', 'tr', 'Mardin', 'Province').
	subdivision('tr-48', 'tr', 'Muğla', 'Province').
	subdivision('tr-49', 'tr', 'Muş', 'Province').
	subdivision('tr-50', 'tr', 'Nevşehir', 'Province').
	subdivision('tr-51', 'tr', 'Niğde', 'Province').
	subdivision('tr-52', 'tr', 'Ordu', 'Province').
	subdivision('tr-53', 'tr', 'Rize', 'Province').
	subdivision('tr-54', 'tr', 'Sakarya', 'Province').
	subdivision('tr-55', 'tr', 'Samsun', 'Province').
	subdivision('tr-56', 'tr', 'Siirt', 'Province').
	subdivision('tr-57', 'tr', 'Sinop', 'Province').
	subdivision('tr-58', 'tr', 'Sivas', 'Province').
	subdivision('tr-59', 'tr', 'Tekirdağ', 'Province').
	subdivision('tr-60', 'tr', 'Tokat', 'Province').
	subdivision('tr-61', 'tr', 'Trabzon', 'Province').
	subdivision('tr-62', 'tr', 'Tunceli', 'Province').
	subdivision('tr-63', 'tr', 'Şanlıurfa', 'Province').
	subdivision('tr-64', 'tr', 'Uşak', 'Province').
	subdivision('tr-65', 'tr', 'Van', 'Province').
	subdivision('tr-66', 'tr', 'Yozgat', 'Province').
	subdivision('tr-67', 'tr', 'Zonguldak', 'Province').
	subdivision('tr-68', 'tr', 'Aksaray', 'Province').
	subdivision('tr-69', 'tr', 'Bayburt', 'Province').
	subdivision('tr-70', 'tr', 'Karaman', 'Province').
	subdivision('tr-71', 'tr', 'Kırıkkale', 'Province').
	subdivision('tr-72', 'tr', 'Batman', 'Province').
	subdivision('tr-73', 'tr', 'Şırnak', 'Province').
	subdivision('tr-74', 'tr', 'Bartın', 'Province').
	subdivision('tr-75', 'tr', 'Ardahan', 'Province').
	subdivision('tr-76', 'tr', 'Iğdır', 'Province').
	subdivision('tr-77', 'tr', 'Yalova', 'Province').
	subdivision('tr-78', 'tr', 'Karabük', 'Province').
	subdivision('tr-79', 'tr', 'Kilis', 'Province').
	subdivision('tr-80', 'tr', 'Osmaniye', 'Province').
	subdivision('tr-81', 'tr', 'Düzce', 'Province').
	subdivision('tt-ari', 'tt', 'Arima', 'Borough').
	subdivision('tt-cha', 'tt', 'Chaguanas', 'Borough').
	subdivision('tt-ctt', 'tt', 'Couva-Tabaquite-Talparo', 'Region').
	subdivision('tt-dmn', 'tt', 'Diego Martin', 'Region').
	subdivision('tt-mrc', 'tt', 'Mayaro-Rio Claro', 'Region').
	subdivision('tt-ped', 'tt', 'Penal-Debe', 'Region').
	subdivision('tt-pos', 'tt', 'Port of Spain', 'City').
	subdivision('tt-prt', 'tt', 'Princes Town', 'Region').
	subdivision('tt-ptf', 'tt', 'Point Fortin', 'Borough').
	subdivision('tt-sfo', 'tt', 'San Fernando', 'City').
	subdivision('tt-sge', 'tt', 'Sangre Grande', 'Region').
	subdivision('tt-sip', 'tt', 'Siparia', 'Region').
	subdivision('tt-sjl', 'tt', 'San Juan-Laventille', 'Region').
	subdivision('tt-tob', 'tt', 'Tobago', 'Ward').
	subdivision('tt-tup', 'tt', 'Tunapuna-Piarco', 'Region').
	subdivision('tv-fun', 'tv', 'Funafuti', 'Town council').
	subdivision('tv-nit', 'tv', 'Niutao', 'Island council').
	subdivision('tv-nkf', 'tv', 'Nukufetau', 'Island council').
	subdivision('tv-nkl', 'tv', 'Nukulaelae', 'Island council').
	subdivision('tv-nma', 'tv', 'Nanumea', 'Island council').
	subdivision('tv-nmg', 'tv', 'Nanumaga', 'Island council').
	subdivision('tv-nui', 'tv', 'Nui', 'Island council').
	subdivision('tv-vai', 'tv', 'Vaitupu', 'Island council').
	subdivision('tw-cha', 'tw', 'Changhua', 'County').
	subdivision('tw-cyi', 'tw', 'Chiayi', 'City').
	subdivision('tw-cyq', 'tw', 'Chiayi', 'County').
	subdivision('tw-hsq', 'tw', 'Hsinchu', 'County').
	subdivision('tw-hsz', 'tw', 'Hsinchu', 'City').
	subdivision('tw-hua', 'tw', 'Hualien', 'County').
	subdivision('tw-ila', 'tw', 'Yilan', 'County').
	subdivision('tw-kee', 'tw', 'Keelung', 'City').
	subdivision('tw-khh', 'tw', 'Kaohsiung', 'Special municipality').
	subdivision('tw-kin', 'tw', 'Kinmen', 'County').
	subdivision('tw-lie', 'tw', 'Lienchiang', 'County').
	subdivision('tw-mia', 'tw', 'Miaoli', 'County').
	subdivision('tw-nan', 'tw', 'Nantou', 'County').
	subdivision('tw-nwt', 'tw', 'New Taipei', 'Special municipality').
	subdivision('tw-pen', 'tw', 'Penghu', 'County').
	subdivision('tw-pif', 'tw', 'Pingtung', 'County').
	subdivision('tw-tao', 'tw', 'Taoyuan', 'Special municipality').
	subdivision('tw-tnn', 'tw', 'Tainan', 'Special municipality').
	subdivision('tw-tpe', 'tw', 'Taipei', 'Special municipality').
	subdivision('tw-ttt', 'tw', 'Taitung', 'County').
	subdivision('tw-txg', 'tw', 'Taichung', 'Special municipality').
	subdivision('tw-yun', 'tw', 'Yunlin', 'County').
	subdivision('tz-01', 'tz', 'Arusha', 'Region').
	subdivision('tz-02', 'tz', 'Dar es Salaam', 'Region').
	subdivision('tz-03', 'tz', 'Dodoma', 'Region').
	subdivision('tz-04', 'tz', 'Iringa', 'Region').
	subdivision('tz-05', 'tz', 'Kagera', 'Region').
	subdivision('tz-06', 'tz', 'Pemba North', 'Region').
	subdivision('tz-07', 'tz', 'Zanzibar North', 'Region').
	subdivision('tz-08', 'tz', 'Kigoma', 'Region').
	subdivision('tz-09', 'tz', 'Kilimanjaro', 'Region').
	subdivision('tz-10', 'tz', 'Pemba South', 'Region').
	subdivision('tz-11', 'tz', 'Zanzibar South', 'Region').
	subdivision('tz-12', 'tz', 'Lindi', 'Region').
	subdivision('tz-13', 'tz', 'Mara', 'Region').
	subdivision('tz-14', 'tz', 'Mbeya', 'Region').
	subdivision('tz-15', 'tz', 'Zanzibar West', 'Region').
	subdivision('tz-16', 'tz', 'Morogoro', 'Region').
	subdivision('tz-17', 'tz', 'Mtwara', 'Region').
	subdivision('tz-18', 'tz', 'Mwanza', 'Region').
	subdivision('tz-19', 'tz', 'Coast', 'Region').
	subdivision('tz-20', 'tz', 'Rukwa', 'Region').
	subdivision('tz-21', 'tz', 'Ruvuma', 'Region').
	subdivision('tz-22', 'tz', 'Shinyanga', 'Region').
	subdivision('tz-23', 'tz', 'Singida', 'Region').
	subdivision('tz-24', 'tz', 'Tabora', 'Region').
	subdivision('tz-25', 'tz', 'Tanga', 'Region').
	subdivision('tz-26', 'tz', 'Manyara', 'Region').
	subdivision('tz-27', 'tz', 'Geita', 'Region').
	subdivision('tz-28', 'tz', 'Katavi', 'Region').
	subdivision('tz-29', 'tz', 'Njombe', 'Region').
	subdivision('tz-30', 'tz', 'Simiyu', 'Region').
	subdivision('tz-31', 'tz', 'Songwe', 'Region').
	subdivision('ua-05', 'ua', 'Vinnytska oblast', 'Region').
	subdivision('ua-07', 'ua', 'Volynska oblast', 'Region').
	subdivision('ua-09', 'ua', 'Luhanska oblast', 'Region').
	subdivision('ua-12', 'ua', 'Dnipropetrovska oblast', 'Region').
	subdivision('ua-14', 'ua', 'Donetska oblast', 'Region').
	subdivision('ua-18', 'ua', 'Zhytomyrska oblast', 'Region').
	subdivision('ua-21', 'ua', 'Zakarpatska oblast', 'Region').
	subdivision('ua-23', 'ua', 'Zaporizka oblast', 'Region').
	subdivision('ua-26', 'ua', 'Ivano-Frankivska oblast', 'Region').
	subdivision('ua-30', 'ua', 'Kyiv', 'City').
	subdivision('ua-32', 'ua', 'Kyivska oblast', 'Region').
	subdivision('ua-35', 'ua', 'Kirovohradska oblast', 'Region').
	subdivision('ua-40', 'ua', 'Sevastopol', 'City').
	subdivision('ua-43', 'ua', 'Avtonomna Respublika Krym', 'Republic').
	subdivision('ua-46', 'ua', 'Lvivska oblast', 'Region').
	subdivision('ua-48', 'ua', 'Mykolaivska oblast', 'Region').
	subdivision('ua-51', 'ua', 'Odeska oblast', 'Region').
	subdivision('ua-53', 'ua', 'Poltavska oblast', 'Region').
	subdivision('ua-56', 'ua', 'Rivnenska oblast', 'Region').
	subdivision('ua-59', 'ua', 'Sumska oblast', 'Region').
	subdivision('ua-61', 'ua', 'Ternopilska oblast', 'Region').
	subdivision('ua-63', 'ua', 'Kharkivska oblast', 'Region').
	subdivision('ua-65', 'ua', 'Khersonska oblast', 'Region').
	subdivision('ua-68', 'ua', 'Khmelnytska oblast', 'Region').
	subdivision('ua-71', 'ua', 'Cherkaska oblast', 'Region').
	subdivision('ua-74', 'ua', 'Chernihivska oblast', 'Region').
	subdivision('ua-77', 'ua', 'Chernivetska oblast', 'Region').
	subdivision('ug-101', 'ug', 'Kalangala', 'District').
	subdivision('ug-102', 'ug', 'Kampala', 'City').
	subdivision('ug-103', 'ug', 'Kiboga', 'District').
	subdivision('ug-104', 'ug', 'Luwero', 'District').
	subdivision('ug-105', 'ug', 'Masaka', 'District').
	subdivision('ug-106', 'ug', 'Mpigi', 'District').
	subdivision('ug-107', 'ug', 'Mubende', 'District').
	subdivision('ug-108', 'ug', 'Mukono', 'District').
	subdivision('ug-109', 'ug', 'Nakasongola', 'District').
	subdivision('ug-110', 'ug', 'Rakai', 'District').
	subdivision('ug-111', 'ug', 'Sembabule', 'District').
	subdivision('ug-112', 'ug', 'Kayunga', 'District').
	subdivision('ug-113', 'ug', 'Wakiso', 'District').
	subdivision('ug-114', 'ug', 'Lyantonde', 'District').
	subdivision('ug-115', 'ug', 'Mityana', 'District').
	subdivision('ug-116', 'ug', 'Nakaseke', 'District').
	subdivision('ug-117', 'ug', 'Buikwe', 'District').
	subdivision('ug-118', 'ug', 'Bukomansibi', 'District').
	subdivision('ug-119', 'ug', 'Butambala', 'District').
	subdivision('ug-120', 'ug', 'Buvuma', 'District').
	subdivision('ug-121', 'ug', 'Gomba', 'District').
	subdivision('ug-122', 'ug', 'Kalungu', 'District').
	subdivision('ug-123', 'ug', 'Kyankwanzi', 'District').
	subdivision('ug-124', 'ug', 'Lwengo', 'District').
	subdivision('ug-125', 'ug', 'Kyotera', 'District').
	subdivision('ug-126', 'ug', 'Kasanda', 'District').
	subdivision('ug-201', 'ug', 'Bugiri', 'District').
	subdivision('ug-202', 'ug', 'Busia', 'District').
	subdivision('ug-203', 'ug', 'Iganga', 'District').
	subdivision('ug-204', 'ug', 'Jinja', 'District').
	subdivision('ug-205', 'ug', 'Kamuli', 'District').
	subdivision('ug-206', 'ug', 'Kapchorwa', 'District').
	subdivision('ug-207', 'ug', 'Katakwi', 'District').
	subdivision('ug-208', 'ug', 'Kumi', 'District').
	subdivision('ug-209', 'ug', 'Mbale', 'District').
	subdivision('ug-210', 'ug', 'Pallisa', 'District').
	subdivision('ug-211', 'ug', 'Soroti', 'District').
	subdivision('ug-212', 'ug', 'Tororo', 'District').
	subdivision('ug-213', 'ug', 'Kaberamaido', 'District').
	subdivision('ug-214', 'ug', 'Mayuge', 'District').
	subdivision('ug-215', 'ug', 'Sironko', 'District').
	subdivision('ug-216', 'ug', 'Amuria', 'District').
	subdivision('ug-217', 'ug', 'Budaka', 'District').
	subdivision('ug-218', 'ug', 'Bududa', 'District').
	subdivision('ug-219', 'ug', 'Bukedea', 'District').
	subdivision('ug-220', 'ug', 'Bukwo', 'District').
	subdivision('ug-221', 'ug', 'Butaleja', 'District').
	subdivision('ug-222', 'ug', 'Kaliro', 'District').
	subdivision('ug-223', 'ug', 'Manafwa', 'District').
	subdivision('ug-224', 'ug', 'Namutumba', 'District').
	subdivision('ug-225', 'ug', 'Bulambuli', 'District').
	subdivision('ug-226', 'ug', 'Buyende', 'District').
	subdivision('ug-227', 'ug', 'Kibuku', 'District').
	subdivision('ug-228', 'ug', 'Kween', 'District').
	subdivision('ug-229', 'ug', 'Luuka', 'District').
	subdivision('ug-230', 'ug', 'Namayingo', 'District').
	subdivision('ug-231', 'ug', 'Ngora', 'District').
	subdivision('ug-232', 'ug', 'Serere', 'District').
	subdivision('ug-233', 'ug', 'Butebo', 'District').
	subdivision('ug-234', 'ug', 'Namisindwa', 'District').
	subdivision('ug-235', 'ug', 'Bugweri', 'District').
	subdivision('ug-236', 'ug', 'Kapelebyong', 'District').
	subdivision('ug-237', 'ug', 'Kalaki', 'District').
	subdivision('ug-301', 'ug', 'Adjumani', 'District').
	subdivision('ug-302', 'ug', 'Apac', 'District').
	subdivision('ug-303', 'ug', 'Arua', 'District').
	subdivision('ug-304', 'ug', 'Gulu', 'District').
	subdivision('ug-305', 'ug', 'Kitgum', 'District').
	subdivision('ug-306', 'ug', 'Kotido', 'District').
	subdivision('ug-307', 'ug', 'Lira', 'District').
	subdivision('ug-308', 'ug', 'Moroto', 'District').
	subdivision('ug-309', 'ug', 'Moyo', 'District').
	subdivision('ug-310', 'ug', 'Nebbi', 'District').
	subdivision('ug-311', 'ug', 'Nakapiripirit', 'District').
	subdivision('ug-312', 'ug', 'Pader', 'District').
	subdivision('ug-313', 'ug', 'Yumbe', 'District').
	subdivision('ug-314', 'ug', 'Abim', 'District').
	subdivision('ug-315', 'ug', 'Amolatar', 'District').
	subdivision('ug-316', 'ug', 'Amuru', 'District').
	subdivision('ug-317', 'ug', 'Dokolo', 'District').
	subdivision('ug-318', 'ug', 'Kaabong', 'District').
	subdivision('ug-319', 'ug', 'Koboko', 'District').
	subdivision('ug-320', 'ug', 'Maracha', 'District').
	subdivision('ug-321', 'ug', 'Oyam', 'District').
	subdivision('ug-322', 'ug', 'Agago', 'District').
	subdivision('ug-323', 'ug', 'Alebtong', 'District').
	subdivision('ug-324', 'ug', 'Amudat', 'District').
	subdivision('ug-325', 'ug', 'Kole', 'District').
	subdivision('ug-326', 'ug', 'Lamwo', 'District').
	subdivision('ug-327', 'ug', 'Napak', 'District').
	subdivision('ug-328', 'ug', 'Nwoya', 'District').
	subdivision('ug-329', 'ug', 'Otuke', 'District').
	subdivision('ug-330', 'ug', 'Zombo', 'District').
	subdivision('ug-331', 'ug', 'Omoro', 'District').
	subdivision('ug-332', 'ug', 'Pakwach', 'District').
	subdivision('ug-333', 'ug', 'Kwania', 'District').
	subdivision('ug-334', 'ug', 'Nabilatuk', 'District').
	subdivision('ug-335', 'ug', 'Karenga', 'District').
	subdivision('ug-336', 'ug', 'Madi-Okollo', 'District').
	subdivision('ug-337', 'ug', 'Obongi', 'District').
	subdivision('ug-401', 'ug', 'Bundibugyo', 'District').
	subdivision('ug-402', 'ug', 'Bushenyi', 'District').
	subdivision('ug-403', 'ug', 'Hoima', 'District').
	subdivision('ug-404', 'ug', 'Kabale', 'District').
	subdivision('ug-405', 'ug', 'Kabarole', 'District').
	subdivision('ug-406', 'ug', 'Kasese', 'District').
	subdivision('ug-407', 'ug', 'Kibaale', 'District').
	subdivision('ug-408', 'ug', 'Kisoro', 'District').
	subdivision('ug-409', 'ug', 'Masindi', 'District').
	subdivision('ug-410', 'ug', 'Mbarara', 'District').
	subdivision('ug-411', 'ug', 'Ntungamo', 'District').
	subdivision('ug-412', 'ug', 'Rukungiri', 'District').
	subdivision('ug-413', 'ug', 'Kamwenge', 'District').
	subdivision('ug-414', 'ug', 'Kanungu', 'District').
	subdivision('ug-415', 'ug', 'Kyenjojo', 'District').
	subdivision('ug-416', 'ug', 'Buliisa', 'District').
	subdivision('ug-417', 'ug', 'Ibanda', 'District').
	subdivision('ug-418', 'ug', 'Isingiro', 'District').
	subdivision('ug-419', 'ug', 'Kiruhura', 'District').
	subdivision('ug-420', 'ug', 'Buhweju', 'District').
	subdivision('ug-421', 'ug', 'Kiryandongo', 'District').
	subdivision('ug-422', 'ug', 'Kyegegwa', 'District').
	subdivision('ug-423', 'ug', 'Mitooma', 'District').
	subdivision('ug-424', 'ug', 'Ntoroko', 'District').
	subdivision('ug-425', 'ug', 'Rubirizi', 'District').
	subdivision('ug-426', 'ug', 'Sheema', 'District').
	subdivision('ug-427', 'ug', 'Kagadi', 'District').
	subdivision('ug-428', 'ug', 'Kakumiro', 'District').
	subdivision('ug-429', 'ug', 'Rubanda', 'District').
	subdivision('ug-430', 'ug', 'Bunyangabu', 'District').
	subdivision('ug-431', 'ug', 'Rukiga', 'District').
	subdivision('ug-432', 'ug', 'Kikuube', 'District').
	subdivision('ug-433', 'ug', 'Kazo', 'District').
	subdivision('ug-434', 'ug', 'Kitagwenda', 'District').
	subdivision('ug-435', 'ug', 'Rwampara', 'District').
	subdivision('ug-c', 'ug', 'Central', 'Geographical region').
	subdivision('ug-e', 'ug', 'Eastern', 'Geographical region').
	subdivision('ug-n', 'ug', 'Northern', 'Geographical region').
	subdivision('ug-w', 'ug', 'Western', 'Geographical region').
	subdivision('um-67', 'um', 'Johnston Atoll', 'Islands, groups of islands').
	subdivision('um-71', 'um', 'Midway Islands', 'Islands, groups of islands').
	subdivision('um-76', 'um', 'Navassa Island', 'Islands, groups of islands').
	subdivision('um-79', 'um', 'Wake Island', 'Islands, groups of islands').
	subdivision('um-81', 'um', 'Baker Island', 'Islands, groups of islands').
	subdivision('um-84', 'um', 'Howland Island', 'Islands, groups of islands').
	subdivision('um-86', 'um', 'Jarvis Island', 'Islands, groups of islands').
	subdivision('um-89', 'um', 'Kingman Reef', 'Islands, groups of islands').
	subdivision('um-95', 'um', 'Palmyra Atoll', 'Islands, groups of islands').
	subdivision('us-ak', 'us', 'Alaska', 'State').
	subdivision('us-al', 'us', 'Alabama', 'State').
	subdivision('us-ar', 'us', 'Arkansas', 'State').
	subdivision('us-as', 'us', 'American Samoa', 'Outlying area').
	subdivision('us-az', 'us', 'Arizona', 'State').
	subdivision('us-ca', 'us', 'California', 'State').
	subdivision('us-co', 'us', 'Colorado', 'State').
	subdivision('us-ct', 'us', 'Connecticut', 'State').
	subdivision('us-dc', 'us', 'District of Columbia', 'District').
	subdivision('us-de', 'us', 'Delaware', 'State').
	subdivision('us-fl', 'us', 'Florida', 'State').
	subdivision('us-ga', 'us', 'Georgia', 'State').
	subdivision('us-gu', 'us', 'Guam', 'Outlying area').
	subdivision('us-hi', 'us', 'Hawaii', 'State').
	subdivision('us-ia', 'us', 'Iowa', 'State').
	subdivision('us-id', 'us', 'Idaho', 'State').
	subdivision('us-il', 'us', 'Illinois', 'State').
	subdivision('us-in', 'us', 'Indiana', 'State').
	subdivision('us-ks', 'us', 'Kansas', 'State').
	subdivision('us-ky', 'us', 'Kentucky', 'State').
	subdivision('us-la', 'us', 'Louisiana', 'State').
	subdivision('us-ma', 'us', 'Massachusetts', 'State').
	subdivision('us-md', 'us', 'Maryland', 'State').
	subdivision('us-me', 'us', 'Maine', 'State').
	subdivision('us-mi', 'us', 'Michigan', 'State').
	subdivision('us-mn', 'us', 'Minnesota', 'State').
	subdivision('us-mo', 'us', 'Missouri', 'State').
	subdivision('us-mp', 'us', 'Northern Mariana Islands', 'Outlying area').
	subdivision('us-ms', 'us', 'Mississippi', 'State').
	subdivision('us-mt', 'us', 'Montana', 'State').
	subdivision('us-nc', 'us', 'North Carolina', 'State').
	subdivision('us-nd', 'us', 'North Dakota', 'State').
	subdivision('us-ne', 'us', 'Nebraska', 'State').
	subdivision('us-nh', 'us', 'New Hampshire', 'State').
	subdivision('us-nj', 'us', 'New Jersey', 'State').
	subdivision('us-nm', 'us', 'New Mexico', 'State').
	subdivision('us-nv', 'us', 'Nevada', 'State').
	subdivision('us-ny', 'us', 'New York', 'State').
	subdivision('us-oh', 'us', 'Ohio', 'State').
	subdivision('us-ok', 'us', 'Oklahoma', 'State').
	subdivision('us-or', 'us', 'Oregon', 'State').
	subdivision('us-pa', 'us', 'Pennsylvania', 'State').
	subdivision('us-pr', 'us', 'Puerto Rico', 'Outlying area').
	subdivision('us-ri', 'us', 'Rhode Island', 'State').
	subdivision('us-sc', 'us', 'South Carolina', 'State').
	subdivision('us-sd', 'us', 'South Dakota', 'State').
	subdivision('us-tn', 'us', 'Tennessee', 'State').
	subdivision('us-tx', 'us', 'Texas', 'State').
	subdivision('us-um', 'us', 'United States Minor Outlying Islands', 'Outlying area').
	subdivision('us-ut', 'us', 'Utah', 'State').
	subdivision('us-va', 'us', 'Virginia', 'State').
	subdivision('us-vi', 'us', 'Virgin Islands, U.S.', 'Outlying area').
	subdivision('us-vt', 'us', 'Vermont', 'State').
	subdivision('us-wa', 'us', 'Washington', 'State').
	subdivision('us-wi', 'us', 'Wisconsin', 'State').
	subdivision('us-wv', 'us', 'West Virginia', 'State').
	subdivision('us-wy', 'us', 'Wyoming', 'State').
	subdivision('uy-ar', 'uy', 'Artigas', 'Department').
	subdivision('uy-ca', 'uy', 'Canelones', 'Department').
	subdivision('uy-cl', 'uy', 'Cerro Largo', 'Department').
	subdivision('uy-co', 'uy', 'Colonia', 'Department').
	subdivision('uy-du', 'uy', 'Durazno', 'Department').
	subdivision('uy-fd', 'uy', 'Florida', 'Department').
	subdivision('uy-fs', 'uy', 'Flores', 'Department').
	subdivision('uy-la', 'uy', 'Lavalleja', 'Department').
	subdivision('uy-ma', 'uy', 'Maldonado', 'Department').
	subdivision('uy-mo', 'uy', 'Montevideo', 'Department').
	subdivision('uy-pa', 'uy', 'Paysandú', 'Department').
	subdivision('uy-rn', 'uy', 'Río Negro', 'Department').
	subdivision('uy-ro', 'uy', 'Rocha', 'Department').
	subdivision('uy-rv', 'uy', 'Rivera', 'Department').
	subdivision('uy-sa', 'uy', 'Salto', 'Department').
	subdivision('uy-sj', 'uy', 'San José', 'Department').
	subdivision('uy-so', 'uy', 'Soriano', 'Department').
	subdivision('uy-ta', 'uy', 'Tacuarembó', 'Department').
	subdivision('uy-tt', 'uy', 'Treinta y Tres', 'Department').
	subdivision('uz-an', 'uz', 'Andijon', 'Region').
	subdivision('uz-bu', 'uz', 'Buxoro', 'Region').
	subdivision('uz-fa', 'uz', 'Farg‘ona', 'Region').
	subdivision('uz-ji', 'uz', 'Jizzax', 'Region').
	subdivision('uz-ng', 'uz', 'Namangan', 'Region').
	subdivision('uz-nw', 'uz', 'Navoiy', 'Region').
	subdivision('uz-qa', 'uz', 'Qashqadaryo', 'Region').
	subdivision('uz-qr', 'uz', 'Qoraqalpog‘iston Respublikasi', 'Republic').
	subdivision('uz-sa', 'uz', 'Samarqand', 'Region').
	subdivision('uz-si', 'uz', 'Sirdaryo', 'Region').
	subdivision('uz-su', 'uz', 'Surxondaryo', 'Region').
	subdivision('uz-tk', 'uz', 'Toshkent', 'City').
	subdivision('uz-to', 'uz', 'Toshkent', 'Region').
	subdivision('uz-xo', 'uz', 'Xorazm', 'Region').
	subdivision('vc-01', 'vc', 'Charlotte', 'Parish').
	subdivision('vc-02', 'vc', 'Saint Andrew', 'Parish').
	subdivision('vc-03', 'vc', 'Saint David', 'Parish').
	subdivision('vc-04', 'vc', 'Saint George', 'Parish').
	subdivision('vc-05', 'vc', 'Saint Patrick', 'Parish').
	subdivision('vc-06', 'vc', 'Grenadines', 'Parish').
	subdivision('ve-a', 've', 'Distrito Capital', 'Capital district').
	subdivision('ve-b', 've', 'Anzoátegui', 'State').
	subdivision('ve-c', 've', 'Apure', 'State').
	subdivision('ve-d', 've', 'Aragua', 'State').
	subdivision('ve-e', 've', 'Barinas', 'State').
	subdivision('ve-f', 've', 'Bolívar', 'State').
	subdivision('ve-g', 've', 'Carabobo', 'State').
	subdivision('ve-h', 've', 'Cojedes', 'State').
	subdivision('ve-i', 've', 'Falcón', 'State').
	subdivision('ve-j', 've', 'Guárico', 'State').
	subdivision('ve-k', 've', 'Lara', 'State').
	subdivision('ve-l', 've', 'Mérida', 'State').
	subdivision('ve-m', 've', 'Miranda', 'State').
	subdivision('ve-n', 've', 'Monagas', 'State').
	subdivision('ve-o', 've', 'Nueva Esparta', 'State').
	subdivision('ve-p', 've', 'Portuguesa', 'State').
	subdivision('ve-r', 've', 'Sucre', 'State').
	subdivision('ve-s', 've', 'Táchira', 'State').
	subdivision('ve-t', 've', 'Trujillo', 'State').
	subdivision('ve-u', 've', 'Yaracuy', 'State').
	subdivision('ve-v', 've', 'Zulia', 'State').
	subdivision('ve-w', 've', 'Dependencias Federales', 'Federal dependency').
	subdivision('ve-x', 've', 'La Guaira', 'State').
	subdivision('ve-y', 've', 'Delta Amacuro', 'State').
	subdivision('ve-z', 've', 'Amazonas', 'State').
	subdivision('vn-01', 'vn', 'Lai Châu', 'Province').
	subdivision('vn-02', 'vn', 'Lào Cai', 'Province').
	subdivision('vn-03', 'vn', 'Hà Giang', 'Province').
	subdivision('vn-04', 'vn', 'Cao Bằng', 'Province').
	subdivision('vn-05', 'vn', 'Sơn La', 'Province').
	subdivision('vn-06', 'vn', 'Yên Bái', 'Province').
	subdivision('vn-07', 'vn', 'Tuyên Quang', 'Province').
	subdivision('vn-09', 'vn', 'Lạng Sơn', 'Province').
	subdivision('vn-13', 'vn', 'Quảng Ninh', 'Province').
	subdivision('vn-14', 'vn', 'Hòa Bình', 'Province').
	subdivision('vn-18', 'vn', 'Ninh Bình', 'Province').
	subdivision('vn-20', 'vn', 'Thái Bình', 'Province').
	subdivision('vn-21', 'vn', 'Thanh Hóa', 'Province').
	subdivision('vn-22', 'vn', 'Nghệ An', 'Province').
	subdivision('vn-23', 'vn', 'Hà Tĩnh', 'Province').
	subdivision('vn-24', 'vn', 'Quảng Bình', 'Province').
	subdivision('vn-25', 'vn', 'Quảng Trị', 'Province').
	subdivision('vn-26', 'vn', 'Thừa Thiên-Huế', 'Province').
	subdivision('vn-27', 'vn', 'Quảng Nam', 'Province').
	subdivision('vn-28', 'vn', 'Kon Tum', 'Province').
	subdivision('vn-29', 'vn', 'Quảng Ngãi', 'Province').
	subdivision('vn-30', 'vn', 'Gia Lai', 'Province').
	subdivision('vn-31', 'vn', 'Bình Định', 'Province').
	subdivision('vn-32', 'vn', 'Phú Yên', 'Province').
	subdivision('vn-33', 'vn', 'Đắk Lắk', 'Province').
	subdivision('vn-34', 'vn', 'Khánh Hòa', 'Province').
	subdivision('vn-35', 'vn', 'Lâm Đồng', 'Province').
	subdivision('vn-36', 'vn', 'Ninh Thuận', 'Province').
	subdivision('vn-37', 'vn', 'Tây Ninh', 'Province').
	subdivision('vn-39', 'vn', 'Đồng Nai', 'Province').
	subdivision('vn-40', 'vn', 'Bình Thuận', 'Province').
	subdivision('vn-41', 'vn', 'Long An', 'Province').
	subdivision('vn-43', 'vn', 'Bà Rịa - Vũng Tàu', 'Province').
	subdivision('vn-44', 'vn', 'An Giang', 'Province').
	subdivision('vn-45', 'vn', 'Đồng Tháp', 'Province').
	subdivision('vn-46', 'vn', 'Tiền Giang', 'Province').
	subdivision('vn-47', 'vn', 'Kiến Giang', 'Province').
	subdivision('vn-49', 'vn', 'Vĩnh Long', 'Province').
	subdivision('vn-50', 'vn', 'Bến Tre', 'Province').
	subdivision('vn-51', 'vn', 'Trà Vinh', 'Province').
	subdivision('vn-52', 'vn', 'Sóc Trăng', 'Province').
	subdivision('vn-53', 'vn', 'Bắc Kạn', 'Province').
	subdivision('vn-54', 'vn', 'Bắc Giang', 'Province').
	subdivision('vn-55', 'vn', 'Bạc Liêu', 'Province').
	subdivision('vn-56', 'vn', 'Bắc Ninh', 'Province').
	subdivision('vn-57', 'vn', 'Bình Dương', 'Province').
	subdivision('vn-58', 'vn', 'Bình Phước', 'Province').
	subdivision('vn-59', 'vn', 'Cà Mau', 'Province').
	subdivision('vn-61', 'vn', 'Hải Dương', 'Province').
	subdivision('vn-63', 'vn', 'Hà Nam', 'Province').
	subdivision('vn-66', 'vn', 'Hưng Yên', 'Province').
	subdivision('vn-67', 'vn', 'Nam Định', 'Province').
	subdivision('vn-68', 'vn', 'Phú Thọ', 'Province').
	subdivision('vn-69', 'vn', 'Thái Nguyên', 'Province').
	subdivision('vn-70', 'vn', 'Vĩnh Phúc', 'Province').
	subdivision('vn-71', 'vn', 'Điện Biên', 'Province').
	subdivision('vn-72', 'vn', 'Đắk Nông', 'Province').
	subdivision('vn-73', 'vn', 'Hậu Giang', 'Province').
	subdivision('vn-ct', 'vn', 'Cần Thơ', 'Municipality').
	subdivision('vn-dn', 'vn', 'Đà Nẵng', 'Municipality').
	subdivision('vn-hn', 'vn', 'Hà Nội', 'Municipality').
	subdivision('vn-hp', 'vn', 'Hải Phòng', 'Municipality').
	subdivision('vn-sg', 'vn', 'Hồ Chí Minh', 'Municipality').
	subdivision('vu-map', 'vu', 'Malampa', 'Province').
	subdivision('vu-pam', 'vu', 'Pénama', 'Province').
	subdivision('vu-sam', 'vu', 'Sanma', 'Province').
	subdivision('vu-see', 'vu', 'Shéfa', 'Province').
	subdivision('vu-tae', 'vu', 'Taféa', 'Province').
	subdivision('vu-tob', 'vu', 'Torba', 'Province').
	subdivision('wf-al', 'wf', 'Alo', 'Administrative precinct').
	subdivision('wf-sg', 'wf', 'Sigave', 'Administrative precinct').
	subdivision('wf-uv', 'wf', 'Uvea', 'Administrative precinct').
	subdivision('ws-aa', 'ws', 'A''ana', 'District').
	subdivision('ws-al', 'ws', 'Aiga-i-le-Tai', 'District').
	subdivision('ws-at', 'ws', 'Atua', 'District').
	subdivision('ws-fa', 'ws', 'Fa''asaleleaga', 'District').
	subdivision('ws-ge', 'ws', 'Gaga''emauga', 'District').
	subdivision('ws-gi', 'ws', 'Gagaifomauga', 'District').
	subdivision('ws-pa', 'ws', 'Palauli', 'District').
	subdivision('ws-sa', 'ws', 'Satupa''itea', 'District').
	subdivision('ws-tu', 'ws', 'Tuamasaga', 'District').
	subdivision('ws-vf', 'ws', 'Va''a-o-Fonoti', 'District').
	subdivision('ws-vs', 'ws', 'Vaisigano', 'District').
	subdivision('ye-ab', 'ye', 'Abyan', 'Governorate').
	subdivision('ye-ad', 'ye', '‘Adan', 'Governorate').
	subdivision('ye-am', 'ye', '‘Amrān', 'Governorate').
	subdivision('ye-ba', 'ye', 'Al Bayḑā’', 'Governorate').
	subdivision('ye-da', 'ye', 'Aḑ Ḑāli‘', 'Governorate').
	subdivision('ye-dh', 'ye', 'Dhamār', 'Governorate').
	subdivision('ye-hd', 'ye', 'Ḩaḑramawt', 'Governorate').
	subdivision('ye-hj', 'ye', 'Ḩajjah', 'Governorate').
	subdivision('ye-hu', 'ye', 'Al Ḩudaydah', 'Governorate').
	subdivision('ye-ib', 'ye', 'Ibb', 'Governorate').
	subdivision('ye-ja', 'ye', 'Al Jawf', 'Governorate').
	subdivision('ye-la', 'ye', 'Laḩij', 'Governorate').
	subdivision('ye-ma', 'ye', 'Ma’rib', 'Governorate').
	subdivision('ye-mr', 'ye', 'Al Mahrah', 'Governorate').
	subdivision('ye-mw', 'ye', 'Al Maḩwīt', 'Governorate').
	subdivision('ye-ra', 'ye', 'Raymah', 'Governorate').
	subdivision('ye-sa', 'ye', 'Amānat al ‘Āşimah [city]', 'Municipality').
	subdivision('ye-sd', 'ye', 'Şāʻdah', 'Governorate').
	subdivision('ye-sh', 'ye', 'Shabwah', 'Governorate').
	subdivision('ye-sn', 'ye', 'Şanʻā’', 'Governorate').
	subdivision('ye-su', 'ye', 'Arkhabīl Suquţrá', 'Governorate').
	subdivision('ye-ta', 'ye', 'Tāʻizz', 'Governorate').
	subdivision('za-ec', 'za', 'Eastern Cape', 'Province').
	subdivision('za-fs', 'za', 'Free State', 'Province').
	subdivision('za-gp', 'za', 'Gauteng', 'Province').
	subdivision('za-kzn', 'za', 'Kwazulu-Natal', 'Province').
	subdivision('za-lp', 'za', 'Limpopo', 'Province').
	subdivision('za-mp', 'za', 'Mpumalanga', 'Province').
	subdivision('za-nc', 'za', 'Northern Cape', 'Province').
	subdivision('za-nw', 'za', 'North-West', 'Province').
	subdivision('za-wc', 'za', 'Western Cape', 'Province').
	subdivision('zm-01', 'zm', 'Western', 'Province').
	subdivision('zm-02', 'zm', 'Central', 'Province').
	subdivision('zm-03', 'zm', 'Eastern', 'Province').
	subdivision('zm-04', 'zm', 'Luapula', 'Province').
	subdivision('zm-05', 'zm', 'Northern', 'Province').
	subdivision('zm-06', 'zm', 'North-Western', 'Province').
	subdivision('zm-07', 'zm', 'Southern', 'Province').
	subdivision('zm-08', 'zm', 'Copperbelt', 'Province').
	subdivision('zm-09', 'zm', 'Lusaka', 'Province').
	subdivision('zm-10', 'zm', 'Muchinga', 'Province').
	subdivision('zw-bu', 'zw', 'Bulawayo', 'Province').
	subdivision('zw-ha', 'zw', 'Harare', 'Province').
	subdivision('zw-ma', 'zw', 'Manicaland', 'Province').
	subdivision('zw-mc', 'zw', 'Mashonaland Central', 'Province').
	subdivision('zw-me', 'zw', 'Mashonaland East', 'Province').
	subdivision('zw-mi', 'zw', 'Midlands', 'Province').
	subdivision('zw-mn', 'zw', 'Matabeleland North', 'Province').
	subdivision('zw-ms', 'zw', 'Matabeleland South', 'Province').
	subdivision('zw-mv', 'zw', 'Masvingo', 'Province').
	subdivision('zw-mw', 'zw', 'Mashonaland West', 'Province').

:- end_object.
