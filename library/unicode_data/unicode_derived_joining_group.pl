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

% Generated from Unicode 17.0.0 UCD data. Do not edit.

unicode_joining_group(CodePoint, Value) :-
	(	var(CodePoint) ->
		unicode_joining_group(Start, End, Value),
		between(Start, End, CodePoint)
	;	unicode_joining_group(Start, End, SpecificValue),
		CodePoint >= Start, CodePoint =< End ->
		Value = SpecificValue
	;	between(0, 1114111, CodePoint),
		Value = 'No_Joining_Group'
	).

unicode_joining_group(1593, 1594, 'Ain').
unicode_joining_group(1696, 1696, 'Ain').
unicode_joining_group(1788, 1788, 'Ain').
unicode_joining_group(1885, 1887, 'Ain').
unicode_joining_group(2227, 2227, 'Ain').
unicode_joining_group(2243, 2243, 'Ain').
unicode_joining_group(1808, 1808, 'Alaph').
unicode_joining_group(1570, 1571, 'Alef').
unicode_joining_group(1573, 1573, 'Alef').
unicode_joining_group(1575, 1575, 'Alef').
unicode_joining_group(1649, 1651, 'Alef').
unicode_joining_group(1653, 1653, 'Alef').
unicode_joining_group(1907, 1908, 'Alef').
unicode_joining_group(2160, 2178, 'Alef').
unicode_joining_group(1576, 1576, 'Beh').
unicode_joining_group(1578, 1579, 'Beh').
unicode_joining_group(1646, 1646, 'Beh').
unicode_joining_group(1657, 1664, 'Beh').
unicode_joining_group(1872, 1878, 'Beh').
unicode_joining_group(2208, 2209, 'Beh').
unicode_joining_group(2230, 2232, 'Beh').
unicode_joining_group(2238, 2240, 'Beh').
unicode_joining_group(1810, 1810, 'Beth').
unicode_joining_group(1837, 1837, 'Beth').
unicode_joining_group(1583, 1584, 'Dal').
unicode_joining_group(1672, 1680, 'Dal').
unicode_joining_group(1774, 1774, 'Dal').
unicode_joining_group(1881, 1882, 'Dal').
unicode_joining_group(2222, 2222, 'Dal').
unicode_joining_group(69314, 69314, 'Dal').
unicode_joining_group(1813, 1814, 'Dalath_Rish').
unicode_joining_group(1834, 1834, 'Dalath_Rish').
unicode_joining_group(1839, 1839, 'Dalath_Rish').
unicode_joining_group(1829, 1829, 'E').
unicode_joining_group(1601, 1601, 'Feh').
unicode_joining_group(1697, 1702, 'Feh').
unicode_joining_group(1888, 1889, 'Feh').
unicode_joining_group(2212, 2212, 'Feh').
unicode_joining_group(1828, 1828, 'Final_Semkath').
unicode_joining_group(1595, 1596, 'Gaf').
unicode_joining_group(1705, 1705, 'Gaf').
unicode_joining_group(1707, 1707, 'Gaf').
unicode_joining_group(1711, 1716, 'Gaf').
unicode_joining_group(1890, 1892, 'Gaf').
unicode_joining_group(2189, 2189, 'Gaf').
unicode_joining_group(2224, 2224, 'Gaf').
unicode_joining_group(2242, 2242, 'Gaf').
unicode_joining_group(2248, 2248, 'Gaf').
unicode_joining_group(1811, 1812, 'Gamal').
unicode_joining_group(1838, 1838, 'Gamal').
unicode_joining_group(1580, 1582, 'Hah').
unicode_joining_group(1665, 1671, 'Hah').
unicode_joining_group(1727, 1727, 'Hah').
unicode_joining_group(1879, 1880, 'Hah').
unicode_joining_group(1902, 1903, 'Hah').
unicode_joining_group(1906, 1906, 'Hah').
unicode_joining_group(1916, 1916, 'Hah').
unicode_joining_group(2186, 2186, 'Hah').
unicode_joining_group(2210, 2210, 'Hah').
unicode_joining_group(2241, 2241, 'Hah').
unicode_joining_group(2245, 2246, 'Hah').
unicode_joining_group(1731, 1731, 'Teh_Marbuta_Goal').
unicode_joining_group(1815, 1815, 'He').
unicode_joining_group(1607, 1607, 'Heh').
unicode_joining_group(1729, 1730, 'Heh_Goal').
unicode_joining_group(1818, 1818, 'Heth').
unicode_joining_group(1603, 1603, 'Kaf').
unicode_joining_group(1708, 1710, 'Kaf').
unicode_joining_group(1919, 1919, 'Kaf').
unicode_joining_group(2228, 2228, 'Kaf').
unicode_joining_group(69316, 69316, 'Kaf').
unicode_joining_group(1823, 1823, 'Kaph').
unicode_joining_group(1726, 1726, 'Knotted_Heh').
unicode_joining_group(1791, 1791, 'Knotted_Heh').
unicode_joining_group(1604, 1604, 'Lam').
unicode_joining_group(1717, 1720, 'Lam').
unicode_joining_group(1898, 1898, 'Lam').
unicode_joining_group(2214, 2214, 'Lam').
unicode_joining_group(2247, 2247, 'Lam').
unicode_joining_group(1824, 1824, 'Lamadh').
unicode_joining_group(1605, 1605, 'Meem').
unicode_joining_group(1893, 1894, 'Meem').
unicode_joining_group(2215, 2215, 'Meem').
unicode_joining_group(1825, 1825, 'Mim').
unicode_joining_group(1606, 1606, 'Noon').
unicode_joining_group(1721, 1724, 'Noon').
unicode_joining_group(1895, 1897, 'Noon').
unicode_joining_group(2185, 2185, 'Noon').
unicode_joining_group(2191, 2191, 'Noon').
unicode_joining_group(1826, 1826, 'Nun').
unicode_joining_group(1830, 1830, 'Pe').
unicode_joining_group(1602, 1602, 'Qaf').
unicode_joining_group(1647, 1647, 'Qaf').
unicode_joining_group(1703, 1704, 'Qaf').
unicode_joining_group(2213, 2213, 'Qaf').
unicode_joining_group(2229, 2229, 'Qaf').
unicode_joining_group(1833, 1833, 'Qaph').
unicode_joining_group(1585, 1586, 'Reh').
unicode_joining_group(1681, 1689, 'Reh').
unicode_joining_group(1775, 1775, 'Reh').
unicode_joining_group(1883, 1883, 'Reh').
unicode_joining_group(1899, 1900, 'Reh').
unicode_joining_group(1905, 1905, 'Reh').
unicode_joining_group(2218, 2218, 'Reh').
unicode_joining_group(2226, 2226, 'Reh').
unicode_joining_group(2233, 2233, 'Reh').
unicode_joining_group(1831, 1831, 'Reversed_Pe').
unicode_joining_group(1589, 1590, 'Sad').
unicode_joining_group(1693, 1694, 'Sad').
unicode_joining_group(1787, 1787, 'Sad').
unicode_joining_group(2223, 2223, 'Sad').
unicode_joining_group(1832, 1832, 'Sadhe').
unicode_joining_group(1587, 1588, 'Seen').
unicode_joining_group(1690, 1692, 'Seen').
unicode_joining_group(1786, 1786, 'Seen').
unicode_joining_group(1884, 1884, 'Seen').
unicode_joining_group(1901, 1901, 'Seen').
unicode_joining_group(1904, 1904, 'Seen').
unicode_joining_group(1917, 1918, 'Seen').
unicode_joining_group(1827, 1827, 'Semkath').
unicode_joining_group(1835, 1835, 'Shin').
unicode_joining_group(1706, 1706, 'Swash_Kaf').
unicode_joining_group(1591, 1592, 'Tah').
unicode_joining_group(1695, 1695, 'Tah').
unicode_joining_group(2187, 2188, 'Tah').
unicode_joining_group(2211, 2211, 'Tah').
unicode_joining_group(69315, 69315, 'Tah').
unicode_joining_group(1836, 1836, 'Taw').
unicode_joining_group(1577, 1577, 'Teh_Marbuta').
unicode_joining_group(1728, 1728, 'Teh_Marbuta').
unicode_joining_group(1749, 1749, 'Teh_Marbuta').
unicode_joining_group(1819, 1820, 'Teth').
unicode_joining_group(1572, 1572, 'Waw').
unicode_joining_group(1608, 1608, 'Waw').
unicode_joining_group(1654, 1655, 'Waw').
unicode_joining_group(1732, 1739, 'Waw').
unicode_joining_group(1743, 1743, 'Waw').
unicode_joining_group(1912, 1913, 'Waw').
unicode_joining_group(2219, 2219, 'Waw').
unicode_joining_group(1816, 1816, 'Syriac_Waw').
unicode_joining_group(1574, 1574, 'Yeh').
unicode_joining_group(1609, 1610, 'Yeh').
unicode_joining_group(1656, 1656, 'Yeh').
unicode_joining_group(1744, 1745, 'Yeh').
unicode_joining_group(1911, 1911, 'Yeh').
unicode_joining_group(2216, 2217, 'Yeh').
unicode_joining_group(2234, 2234, 'Yeh').
unicode_joining_group(69319, 69319, 'Yeh').
unicode_joining_group(1746, 1747, 'Yeh_Barree').
unicode_joining_group(1741, 1741, 'Yeh_With_Tail').
unicode_joining_group(1821, 1821, 'Yudh').
unicode_joining_group(1822, 1822, 'Yudh_He').
unicode_joining_group(1817, 1817, 'Zain').
unicode_joining_group(1869, 1869, 'Zhain').
unicode_joining_group(1870, 1870, 'Khaph').
unicode_joining_group(1871, 1871, 'Fe').
unicode_joining_group(1914, 1915, 'Burushaski_Yeh_Barree').
unicode_joining_group(1597, 1599, 'Farsi_Yeh').
unicode_joining_group(1740, 1740, 'Farsi_Yeh').
unicode_joining_group(1742, 1742, 'Farsi_Yeh').
unicode_joining_group(1909, 1910, 'Farsi_Yeh').
unicode_joining_group(1725, 1725, 'Nya').
unicode_joining_group(2220, 2220, 'Rohingya_Yeh').
unicode_joining_group(2225, 2225, 'Straight_Waw').
unicode_joining_group(68288, 68288, 'Manichaean_Aleph').
unicode_joining_group(68313, 68314, 'Manichaean_Ayin').
unicode_joining_group(68289, 68290, 'Manichaean_Beth').
unicode_joining_group(68293, 68293, 'Manichaean_Daleth').
unicode_joining_group(68308, 68308, 'Manichaean_Dhamedh').
unicode_joining_group(68332, 68332, 'Manichaean_Five').
unicode_joining_group(68291, 68292, 'Manichaean_Gimel').
unicode_joining_group(68301, 68301, 'Manichaean_Heth').
unicode_joining_group(68335, 68335, 'Manichaean_Hundred').
unicode_joining_group(68304, 68306, 'Manichaean_Kaph').
unicode_joining_group(68307, 68307, 'Manichaean_Lamedh').
unicode_joining_group(68310, 68310, 'Manichaean_Mem').
unicode_joining_group(68311, 68311, 'Manichaean_Nun').
unicode_joining_group(68331, 68331, 'Manichaean_One').
unicode_joining_group(68315, 68316, 'Manichaean_Pe').
unicode_joining_group(68318, 68320, 'Manichaean_Qoph').
unicode_joining_group(68321, 68321, 'Manichaean_Resh').
unicode_joining_group(68317, 68317, 'Manichaean_Sadhe').
unicode_joining_group(68312, 68312, 'Manichaean_Samekh').
unicode_joining_group(68324, 68324, 'Manichaean_Taw').
unicode_joining_group(68333, 68333, 'Manichaean_Ten').
unicode_joining_group(68302, 68302, 'Manichaean_Teth').
unicode_joining_group(68309, 68309, 'Manichaean_Thamedh').
unicode_joining_group(68334, 68334, 'Manichaean_Twenty').
unicode_joining_group(68295, 68295, 'Manichaean_Waw').
unicode_joining_group(68303, 68303, 'Manichaean_Yodh').
unicode_joining_group(68297, 68298, 'Manichaean_Zayin').
unicode_joining_group(2235, 2235, 'African_Feh').
unicode_joining_group(2236, 2236, 'African_Qaf').
unicode_joining_group(2244, 2244, 'African_Qaf').
unicode_joining_group(2237, 2237, 'African_Noon').
unicode_joining_group(2144, 2144, 'Malayalam_Nga').
unicode_joining_group(2145, 2145, 'Malayalam_Ja').
unicode_joining_group(2146, 2146, 'Malayalam_Nya').
unicode_joining_group(2147, 2147, 'Malayalam_Tta').
unicode_joining_group(2148, 2148, 'Malayalam_Nna').
unicode_joining_group(2149, 2149, 'Malayalam_Nnna').
unicode_joining_group(2150, 2150, 'Malayalam_Bha').
unicode_joining_group(2151, 2151, 'Malayalam_Ra').
unicode_joining_group(2152, 2152, 'Malayalam_Lla').
unicode_joining_group(2153, 2153, 'Malayalam_Llla').
unicode_joining_group(2154, 2154, 'Malayalam_Ssa').
unicode_joining_group(68866, 68866, 'Hanifi_Rohingya_Pa').
unicode_joining_group(68873, 68873, 'Hanifi_Rohingya_Pa').
unicode_joining_group(68892, 68892, 'Hanifi_Rohingya_Pa').
unicode_joining_group(68889, 68889, 'Hanifi_Rohingya_Kinna_Ya').
unicode_joining_group(68894, 68894, 'Hanifi_Rohingya_Kinna_Ya').
unicode_joining_group(68896, 68896, 'Hanifi_Rohingya_Kinna_Ya').
unicode_joining_group(68899, 68899, 'Hanifi_Rohingya_Kinna_Ya').
unicode_joining_group(2182, 2182, 'Thin_Yeh').
unicode_joining_group(2190, 2190, 'Vertical_Tail').
unicode_joining_group(1568, 1568, 'Kashmiri_Yeh').
unicode_joining_group(69318, 69318, 'Thin_Noon').
