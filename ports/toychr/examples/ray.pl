/*
    ray.pl -- ray tracing example
    Copyright (C) 2003 Gregory J. Duck

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

/*
    Note: See `ray.chr' for documentation.
*/

scene(File) :-
	^^chr_is(
		_,
		(	eye(50.0),
			light(0.0,0.0,15.0,color(1.0,1.0,0.5)),
			light(0.0,0.0,100.0,color(0.3,0.3,1.0)),
			light(100.0,0.0,15.0,color(1.0,0.5,0.5)),
			sphere(7.0,7.0,3.0,1.0,color(1.0,0.0,0.0)),
			sphere(-11.0,-11.0,-10.0,13.0,color(0.0,1.0,0.0)),
			sphere(-14.0,-10.0,3.0,3.0,color(0.0,0.0,1.0)),
			sphere(-5.0,-3.0,8.0,1.5,color(1.0,1.0,0.0)),
			sphere(-5.0,4.0,2.0,5.0,color(0.0,1.0,1.0)),
			sphere(-5.0,3.0,13.0,1.0,color(0.0,1.0,0.5)),
			sphere(5.5,-3.5,7.5,3.0,color(0.5,0.0,1.0)),
			plane(0.0,0.0,-10.0,0.0,color(1.0,1.0,1.0)),
			draw_scene(File)
		)
	).
