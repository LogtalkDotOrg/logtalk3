%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


/*
Databases are object databases represented by a predicate specified by the user.
A database is specified using the format Object::Functor, where Functor is the
functor of the object predicate holding the spreadsheet information. The arity
of the corresponding predicate is implicit and equal to five:

	Functor(SheetIndex, RowIndex, ColumnIndex, CellType, CellValue)

In addition, some metadata on each spreadsheet sheet is represented by:

	Functor(SheetName, SheetIndex, FirstRow, LastRow)

The object (which can be the pseudo-object "user") must exist prior to calling
the "spreadsheet" object main predicates.
*/

:- object(spreadsheet).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura and Ebrahim Azarisooreh',
		date is 2025-03-11,
		comment is 'Predicates for reading and writing Excel spreadsheet files.',
		remarks is [
			'Spreadsheet objects' - 'Objects that represent the contents of a spreadsheet file using a chosen functor to represent sheet and cell data.',
			'Spreadsheet object sheet representation' - '``Functor(SheetName, SheetIndex, FirstRow, LastRow)``.',
			'Spreadsheet object cell representation' - '``Functor(SheetIndex, RowIndex, ColumnIndex, CellType, CellValue)``.',
			'Spreadsheet file types' - 'Both ``.xls`` and ``.xlsx`` files are supported.',
			'Spreadsheet cell types' - 'The cell types are ``{numeric, string, boolean, blank, formula, error}``. Currently, the only usable types are ``{numeric, string, boolean}``. Booleans are represented by ``{true, false}``. Strings are represented by atoms.'
		]
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(integer, [
		between/3
	]).

	:- public(load/3).
	:- mode(load(+atom, +object_identifier, +atom), one).
	:- info(load/3, [
		comment is 'Loads a spreadsheet into memory, generating an object database where the predicates have the given functor. The spreadsheet must be specified using a full path and using either ``.xls`` or ``.xlsx`` file name extensions.',
		argnames is ['Spreadsheet', 'Object', 'Functor'],
		exceptions is [
			'Spreadsheet is not instantiated' - instantiation_error,
			'Object is not instantiated' - instantiation_error,
			'Functor is not instantiated' - instantiation_error,
			'Spreadsheet does not exist' - existence_error(file, 'Spreadsheet'),
			'Spreadsheet is neither a .xls nor a .xlsx file' - domain_error(file(['.xls', '.xlsx']), 'Spreadsheet'),
			'Object does not exist' - existence_error(object, 'Object'),
			'Functor is neither a variable nor an atom' - type_error(atom, 'Functor')
		]
	]).

	:- public(save/3).
	:- mode(save(+object_identifier, +atom, +atom), one).
	:- info(save/3, [
		comment is 'Saves an object database where the predicates have the given functor to a spreadsheet. The spreadsheet must be specified using a full path and using either ``.xls`` or ``.xlsx`` file name extensions.',
		argnames is ['Object', 'Functor', 'Spreadsheet'],
		exceptions is [
			'Spreadsheet is not instantiated' - instantiation_error,
			'Object is not instantiated' - instantiation_error,
			'Functor is not instantiated' - instantiation_error,
			'Spreadsheet neither a variable nor an atom' - type_error(atom, 'Spreadsheet'),
			'Spreadsheet file name extension is neither .xls nor .xlsx' - domain_error(one_of(atom,['.xls', '.xlsx']), 'Extension'),
			'Object does not exist' - existence_error(object, 'Object'),
			'Functor is neither a variable nor an atom' - type_error(atom, 'Functor'),
			'Functor/4 is not a public predicate of Object' - domain_error(_,'Functor'),
			'Functor/5 is not a public predicate of Object' - domain_error(_,'Functor')
		]
	]).

	:- public(valid/2).
	:- mode(valid(+object_identifier, +atom), zero_or_one).
	:- info(valid/2, [
		comment is 'Verifies that an object database meta-data and data are valid.',
		argnames is ['Object', 'Functor']
	]).

	load(Spreadsheet, Object, Functor) :-
		context(Context),
		check(file(['.xls', '.xlsx']), Spreadsheet, Context),
		check(object, Object, Context),
		check(atom, Functor, Context),
		spreadsheet_type(Spreadsheet, Type),
		java('java.io.FileInputStream')::new([Spreadsheet], Stream),
		create_workbook(Type, Stream, WorkBook),
		java(Stream)::close,
		java(WorkBook, NumberOfSheets)::getNumberOfSheets,
		workbook_to_sheets(WorkBook, NumberOfSheets, Sheets),
		load_sheets(Sheets, WorkBook, Object, Functor).

	spreadsheet_type(Spreadsheet, Type) :-
		(	sub_atom(Spreadsheet, _, 4, 0, '.xls') ->
			Type = hssf
		;	% sub_atom(Spreadsheet, _, 5, 0, '.xlsx'),
			Type = xssf
		).

	create_workbook(hssf, Stream, WorkBook) :-
		java('org.apache.poi.ss.usermodel.WorkbookFactory', WorkBook)::create(Stream).
	create_workbook(xssf, Stream, WorkBook) :-
		java('org.apache.poi.ss.usermodel.WorkbookFactory', WorkBook)::create(Stream).

	workbook_to_sheets(WorkBook, NumSheets, Sheets) :-
		workbook_to_sheets(WorkBook, 0, NumSheets, Sheets).

	workbook_to_sheets(_, NumSheets, NumSheets, []) :-
		!.
	workbook_to_sheets(WorkBook, NumSheets0, NumSheets, [Sheet| Sheets]) :-
		java(WorkBook, Sheet)::getSheetAt(NumSheets0),
		NumSheets1 is NumSheets0 + 1,
		workbook_to_sheets(WorkBook, NumSheets1, NumSheets, Sheets).

	load_sheets([], _, _, _).
	load_sheets([Sheet| Sheets], WorkBook, Object, Functor) :-
		load_sheet(Sheet, WorkBook, Object, Functor),
		load_sheets(Sheets, WorkBook, Object, Functor).

	load_sheet(Sheet, WorkBook, Object, Functor) :-
		java(Sheet, SheetName)::getSheetName,
		java(WorkBook, SheetIndex)::getSheetIndex(SheetName),
		java(Sheet, FirstRow)::getFirstRowNum,
		java(Sheet, LastRow)::getLastRowNum,
		load_sheet_metadata(SheetName, SheetIndex, FirstRow, LastRow, Object, Functor),
		load_sheet_rows(Sheet, SheetIndex, Object, Functor).

	load_sheet_metadata(SheetName, SheetIndex, FirstRow, LastRow, Object, Functor) :-
		Clause =.. [Functor, SheetName, SheetIndex, FirstRow, LastRow],
		(	Object == user ->
			% just for optimization
			{assertz(Clause)}
		;	Object::assertz(Clause)
		).

	load_sheet_rows(Sheet, SheetIndex, Object, Functor) :-
		java(Sheet, Iterator)::rowIterator,
		java::iterator_element(Iterator, Row),
			load_sheet_row(SheetIndex, Row, Object, Functor),
		fail.
	load_sheet_rows(_, _, _, _).

	load_sheet_row(SheetIndex, Row, Object, Functor) :-
		java(Row, RowIndex)::getRowNum,
		java(Row, Iterator)::iterator,
		java::iterator_element(Iterator, Cell),
			java(Cell, CellType)::getCellType,
			java(CellType, CellTypeName)::toString,
			java(Cell, ColumnIndex)::getColumnIndex,
			load_sheet_row_cell(CellTypeName, Cell, SheetIndex, RowIndex, ColumnIndex, Object, Functor),
		fail.
	load_sheet_row(_, _, _, _).

	load_sheet_row_cell('NUMERIC', Cell, SheetIndex, RowIndex, ColumnIndex, Object, Functor) :-
		java(Cell, CellValue)::getNumericCellValue,
		assert_sheet_row_cell(Object, Functor, SheetIndex, RowIndex, ColumnIndex, numeric, CellValue).
	load_sheet_row_cell('STRING', Cell, SheetIndex, RowIndex, ColumnIndex, Object, Functor) :-
		java(Cell, CellValue)::getStringCellValue,
		assert_sheet_row_cell(Object, Functor, SheetIndex, RowIndex, ColumnIndex, string, CellValue).
	load_sheet_row_cell('FORMULA', Cell, SheetIndex, RowIndex, ColumnIndex, Object, Functor) :-
		java(Cell, CellValue)::getCellFormula,
		assert_sheet_row_cell(Object, Functor, SheetIndex, RowIndex, ColumnIndex, formula, CellValue).
	load_sheet_row_cell('BLANK', _Cell, _SheetIndex, _RowIndex, _ColumnIndex, _Object, _Functor).
	load_sheet_row_cell('BOOLEAN', Cell, SheetIndex, RowIndex, ColumnIndex, Object, Functor) :-
		java(Cell, CellValue0)::getBooleanCellValue,
		(	java::is_true(CellValue0) ->
			CellValue = true
		;	CellValue = false
		),
		assert_sheet_row_cell(Object, Functor, SheetIndex, RowIndex, ColumnIndex, boolean, CellValue).
	load_sheet_row_cell('ERROR', Cell, SheetIndex, RowIndex, ColumnIndex, Object, Functor) :-
		java(Cell, CellValue)::getErrorCellString,
		assert_sheet_row_cell(Object, Functor, SheetIndex, RowIndex, ColumnIndex, error, CellValue).

	assert_sheet_row_cell(Object, Functor, SheetIndex, RowIndex, ColumnIndex, CellType, CellValue) :-
		Clause =.. [Functor, SheetIndex, RowIndex, ColumnIndex, CellType, CellValue],
		(	Object == user ->
			% small performance optimization
			{assertz(Clause)}
		;	Object::assertz(Clause)
		).

	save(Object, Functor, Spreadsheet) :-
		context(Context),
		check(object, Object, Context),
		check(predicate, Object::Functor/4, Context),
		check(predicate, Object::Functor/5, Context),
		check(atom, Spreadsheet, Context),
		os::decompose_file_name(Spreadsheet, _, _, Extension),
		check(one_of(atom, ['.xls', '.xlsx']), Extension, Context),
		% create the workbook for creating the sheets
		spreadsheet_type(Spreadsheet, Type),
		(	Type == hssf ->
			java('org.apache.poi.hssf.usermodel.HSSFWorkbook')::new(Workbook)
		;	%Type == xssf,
			java('org.apache.poi.xssf.usermodel.XSSFWorkbook')::new(Workbook)
		),
		(	Metadata =.. [Functor, SheetName, SheetIndex, FirstRow, LastRow],
			Object::Metadata,
				java(Workbook, Sheet)::createSheet(SheetName),
				save_rows(Object, Functor, Sheet, SheetIndex, FirstRow, LastRow),
			fail
		;	java('java.io.FileOutputStream')::new([Spreadsheet], Stream),
			java(Workbook)::write(Stream),
			java(Stream)::close
		).

	save_rows(Object, Functor, Sheet, SheetIndex, RowIndex, LastRow) :-
		(	RowIndex =< LastRow ->
			java(Sheet, Row)::createRow(RowIndex),
			save_row_cells(Object, Functor, SheetIndex, RowIndex, Row),
			NextRowIndex is RowIndex + 1,
			save_rows(Object, Functor, Sheet, SheetIndex, NextRowIndex, LastRow)
		;	true
		).

	save_row_cells(Object, Functor, SheetIndex, RowIndex, Row) :-
		Data =.. [Functor, SheetIndex, RowIndex, ColumnIndex, CellTypeName0, CellValue0],
		Object::Data,
		convert_cell_data(CellTypeName0, CellValue0, CellTypeName, CellValue),
			java(Row, Cell)::createCell(ColumnIndex),
			java('org.apache.poi.ss.usermodel.CellType', CellType)::valueOf(CellTypeName),
			java(Cell)::setCellType(CellType),
			java(Cell)::setCellValue(CellValue),
		fail.
	save_row_cells(_, _, _, _, _).

	convert_cell_data(numeric, CellValue,  'NUMERIC', CellValue).
	convert_cell_data(string,  CellValue,  'STRING',  CellValue).
	convert_cell_data(formula, CellValue,  'FORMULA', CellValue).
	convert_cell_data(blank,   CellValue,  'BLANK',   CellValue).
	convert_cell_data(boolean, CellValue0, 'BOOLEAN', CellValue) :-
		java::value_reference(CellValue0, CellValue).
	convert_cell_data(error,   CellValue,  'ERROR',   CellValue).

	valid(Object, Functor) :-
		Object::current_predicate(Functor/4),
		Object::current_predicate(Functor/5),
		valid_data(Object, Functor).

	valid_data(Object, Functor) :-
		Metadata =.. [Functor, SheetName, SheetIndex, FirstRow, LastRow],
		forall(
			Object::Metadata,
			(	atom(SheetName),
				integer(SheetIndex), SheetIndex >= 0,
				integer(FirstRow), FirstRow >= 0,
				integer(LastRow), LastRow >= 0, LastRow >= FirstRow,
				valid_data(Object, Functor, SheetIndex, FirstRow, LastRow)
			)
		).

	valid_data(Object, Functor, SheetIndex, FirstRow, LastRow) :-
		forall(
			between(FirstRow, LastRow, RowIndex),
			valid_row(Object, Functor, SheetIndex, RowIndex)
		).

	valid_row(Object, Functor, SheetIndex, RowIndex) :-
		Data =.. [Functor, SheetIndex, RowIndex, ColumnIndex, CellTypeName, CellValue],
		forall(
			Object::Data,
			(	integer(ColumnIndex), ColumnIndex >= 0,
				valid_cell_type(CellTypeName),
				valid_cell_value(CellTypeName, CellValue)
			)
		).

	valid_cell_type(numeric).
	valid_cell_type(string).
	valid_cell_type(boolean).

	valid_cell_value(numeric, CellValue) :-
		number(CellValue).
	valid_cell_value(string, CellValue) :-
		atom(CellValue).
	valid_cell_value(boolean, CellValue) :-
		(	CellValue == true ->
			true
		;	CellValue == false
	).

:- end_object.
