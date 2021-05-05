/*
    ray.chr -- a simple ray tracer
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
    How to render a scene:

    1) Start Prolog & load toychr
    2) ?- ['ray.pl'].
    3) ?- chr_compile('ray.chr').
    4) ?- scene('out.ppm') creates image 'out.ppm'

    Notes:
        -) Colors are represented by the term color(R,G,B), where
                0.0 =< R,G,B =< 1.0
        -) Must specify an eye point via constraint eye(Z).  The
                camera is set at location (0,0,Z).
        -) Light sources are specified via constraint light(X,Y,Z,C)
                where (X,Y,Z) is the location and C is the color.
        -) Objects are:
                1) sphere(X,Y,Z,R,C) -- draws a sphere at (X,Y,Z)
                with radius R and color C.
                2) plane(A,B,C,D,Cl) -- draws a plane given by the
                equation A.X + B.Y + C.Z + D = 0 and color Cl
        -) Use draw_scene(+File) to render the scene (`File' is the
                output file).
        -) See the example in 'ray.pl'.
*/

:- set_logtalk_flag(hook, toychrdb).


:- object(ray,
	extends(toychrdb)).

	:- public(scene/1).

	chr_option_allow_deep_guards.

	:- include('ray.pl').

	draw_scene(File) <=>
		open(File, write, Stream),
		set_output(Stream),
		format('P3\n512 512\n255\n',[]),
		render_pixels(0,0),
		close(Stream).

	render_pixels(X,Y) <=>
		(	Y = 512 ->
			true
		;	pixel(X,Y,C),
			C = color(R,G,B),
			Ri is floor(255.0*R),
			Gi is floor(255.0*G),
			Bi is floor(255.0*B),
			format('~d ~d ~d ',[Ri,Gi,Bi]),
			NX is X + 1,
			(	NX = 512 ->
				nl,
				NY is Y + 1,
				render_pixels(0,NY)
			;	render_pixels(NX,Y)
			)
		).

	sphere_intersection_calculation(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,R,U1,U2) <=>
		A is (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1) + (Z2-Z1)*(Z2-Z1),
		B is 2.0*((X2-X1)*(X1-X3) + (Y2-Y1)*(Y1-Y3) + (Z2-Z1)*(Z1-Z3)),
		C is X3*X3 + Y3*Y3 + Z3*Z3 + X1*X1 + Y1*Y1 + Z1*Z1 - 2.0*(X3*X1 + Y3*Y1 + Z3*Z1) - R*R,
		D is B*B - 4.0*A*C,
		D >= 0,
		U1 is (-B + sqrt(D))/(2.0*A),
		U2 is (-B - sqrt(D))/(2.0*A).

	plane_intersection_calculation(X1,Y1,Z1,X2,Y2,Z2,A,B,C,D,U) <=>
		UD is A*(X1-X2) + B*(Y1-Y2) + C*(Z1-Z2),
		UD \= 0.0,
		UN is A*X1 + B*Y1 + C*Z1 + D,
		U is UN/UD.

	intersection_point(X1,Y1,Z1,X2,Y2,Z2,U,PX,PY,PZ) <=>
		PX is X1 + U*(X2-X1),
		PY is Y1 + U*(Y2-Y1),
		PZ is Z1 + U*(Z2-Z1).

	distance(X1,Y1,Z1,X2,Y2,Z2,D) <=>
		DX is X1-X2,
		DY is Y1-Y2,
		DZ is Z1-Z2,
		D is sqrt(DX*DX + DY*DY + DZ*DZ).

	normalized_vector(X1,Y1,Z1,X2,Y2,Z2,VX,VY,VZ) <=>
		distance(X1,Y1,Z1,X2,Y2,Z2,L),
		IL is 1.0/L,
		VX is IL*(X2-X1),
		VY is IL*(Y2-Y1),
		VZ is IL*(Z2-Z1).

	normalized_plane_vector(A,B,C,D,VX,VY,VZ) <=>
		VX0 is A+D,
		VY0 is B+D,
		VZ0 is C+D,
		L is sqrt(VX0*VX0 + VY0*VY0 + VZ0*VZ0),
		IL is 1.0/L,
		VX is IL*VX0,
		VY is IL*VY0,
		VZ is IL*VZ0.

	dot_product(X1,Y1,Z1,X2,Y2,Z2,D) <=>
		D is X1*X2 + Y1*Y2 + Z1*Z2.

	sphere_blocks(U1,U2) <=>
		(	U1 > 0.0, U1 < 1.0 ->
			true
		;	U2 > 0.0, U2 < 1.0 ->
			true
		;	fail
		).

	plane_blocks(U) <=>
		U > 0.0,
		U < 1.0.

	blend_colors(D,C1,C2,C3) <=>
		C1 = color(R1,G1,B1),
		C2 = color(R2,G2,B2),
		R3 is D*R1*R2,
		G3 is D*G1*G2,
		B3 is D*B1*B2,
		C3 = color(R3,G3,B3).

	object_id(_) \ object_id(_) <=>
		true.
	object_id(I), sphere(X,Y,Z,R,C) <=>
		sphere(I,X,Y,Z,R,C),
		J is I+1,
		object_id(J).
	sphere(X,Y,Z,R,C) <=>
		sphere(0,X,Y,Z,R,C),
		object_id(1).
	object_id(I), plane(A,B,C,D,Cl) <=>
		plane(I,A,B,C,D,Cl),
		J is I+1,
		object_id(J).
	plane(A,B,C,D,Cl) <=>
		plane(0,A,B,C,D,Cl),
		object_id(1).

	eye(_) \ eye(_) <=>
		true.

	%%% Cast a ray for a pixel

	pixel(_,_,_) ==>
		color(color(0.0,0.0,0.0)).
	pixel(X,Y,_), eye(Z) ==>
		DX is 0.5 - X/512.0,
		DY is 0.5 - Y/512.0,
		DZ is Z - 1.0,
		ray(0.0,0.0,Z,DX,DY,DZ).
	pixel(_,_,_), light(LX,LY,LZ,C) ==>
		light_ray(LX,LY,LZ,C).
	pixel(_,_,C) <=>
		clean_up,
		get_color(C).

	color(_) \ color(_) <=>
		true.
	color(C1), get_color(C2) <=>
		C1 = color(R1,G1,B1),
		(	R1 > 1.0 ->
			R2 = 1.0
		;	R2 = R1
		),
		(	G1 > 1.0 ->
			G2 = 1.0
		;	G2 = G1
		),
		(	B1 > 1.0 ->
			B2 = 1.0
		;	B2 = B1
		),
		C2 = color(R2,G2,B2).

	get_color(C) <=>
		C = color(0.0,0.0,0.0).

	%%% Calculate intersection.

	ray(X1,Y1,Z1,X2,Y2,Z2), sphere(I,X3,Y3,Z3,R,C) ==>
		(	sphere_intersection_calculation(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,R,U1,U2) ->
			(	U1 > 0.0 ->
				intersection_point(X1,Y1,Z1,X2,Y2,Z2,U1,PX1,PY1,PZ1),
				distance(X1,Y1,Z1,PX1,PY1,PZ1,L1),
				intersection(PX1,PY1,PZ1,L1,I,C)
			;	true
			),
			(	U2 > 0.0 ->
				intersection_point(X1,Y1,Z1,X2,Y2,Z2,U2,PX2,PY2,PZ2),
				distance(X1,Y1,Z1,PX2,PY2,PZ2,L2),
				intersection(PX2,PY2,PZ2,L2,I,C)
			;	true
			)
		;	true
		).
	ray(X1,Y1,Z1,X2,Y2,Z2), plane(I,A,B,C,D,Cl) ==>
		(	plane_intersection_calculation(X1,Y1,Z1,X2,Y2,Z2,A,B,C,D,U),
			U > 0.0 ->
			intersection_point(X1,Y1,Z1,X2,Y2,Z2,U,PX,PY,PZ),
			distance(X1,Y1,Z1,PX,PY,PZ,L),
			intersection(PX,PY,PZ,L,I,Cl)
		;	true
		).
	ray(_,_,_,_,_,_) <=>
		true.

	intersection(X,Y,Z,_,I,_) \ light_ray(LX,LY,LZ,C) <=>
		light_ray(LX,LY,LZ,X,Y,Z,C,I).

	light_ray(_,_,_,_) <=>
		true.

	%%% Calculate shadows

	sphere(I,X3,Y3,Z3,R,_) \ light_ray(X1,Y1,Z1,X2,Y2,Z2,_,J) <=>
		I \= J,
		sphere_intersection_calculation(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,R,U1,U2),
		sphere_blocks(U1,U2) |
			true.

	plane(I,A,B,C,D,_) \ light_ray(X1,Y1,Z1,X2,Y2,Z2,_,J) <=>
		I \= J,
		plane_intersection_calculation(X1,Y1,Z1,X2,Y2,Z2,A,B,C,D,U),
		plane_blocks(U) |
			true.

	%%% Blend colors.

	sphere(I,X3,Y3,Z3,_,C1) \ light_ray(X1,Y1,Z1,X2,Y2,Z2,C2,I) <=>
		normalized_vector(X3,Y3,Z3,X2,Y2,Z2,VX1,VY1,VZ1),
		normalized_vector(X2,Y2,Z2,X1,Y1,Z1,VX2,VY2,VZ2),
		dot_product(VX1,VY1,VZ1,VX2,VY2,VZ2,D),
		(	D =< 0 ->
			true
		;	blend_colors(D,C1,C2,C3),
			add_color(C3)
		).

	plane(I,A,B,C,D,C1) \ light_ray(X1,Y1,Z1,X2,Y2,Z2,C2,I) <=>
		normalized_plane_vector(A,B,C,D,VX1,VY1,VZ1),
		normalized_vector(X2,Y2,Z2,X1,Y1,Z1,VX2,VY2,VZ2),
		dot_product(VX1,VY1,VZ1,VX2,VY2,VZ2,D0),
		D1 is abs(D0),
		blend_colors(D1,C1,C2,C3),
		add_color(C3).

	light_ray(_,_,_,_,_,_,_,_) <=>
		true.

	add_color(C1), color(C2) <=>
		C1 = color(R1,G1,B1),
		C2 = color(R2,G2,B2),
		R3 is R2 + R1,
		G3 is G2 + G1,
		B3 is B2 + B1,
		C3 = color(R3,G3,B3),
		color(C3).
	add_color(_) <=>
		true.

	intersection(_,_,_,L1,_,_) \ intersection(_,_,_,L2,_,_) <=>
		L1 =< L2 |
			true.

	clean_up \ intersection(_,_,_,_,_,_) <=>
		true.
	clean_up <=>
		true.

:- end_object.
