#include <stdio.h>
#include <stdlib.h>
#include <search.h>
#include <string.h>

#ifdef _MSC_VER
	#define unlink _unlink
	typedef __int32 int32_t;
#else
	#include <unistd.h>
	#include <stdint.h>
#endif

#define FREE(p) {if(p) {free(p); p = NULL;}}

/*
#define CLEANUP() {                                                 \
	FREE(buf);                                                  \
	FREE(infilename);                                           \
	FREE(outfilename);                                          \
	for (i = 0; i < allocated_line_count; ++i) FREE(lines[i]);  \
	FREE(lines);                                                \
	for (i = 0; i < tempfile_count; ++i) {                      \
		unlink(temp_filenames[i]);                          \
		FREE(temp_filenames[i]);                            \
		FREE(nextlines[i]);                                 \
	}                                                           \
	FREE(temp_filenames);                                       \
	FREE(tempfiles);                                            \
	FREE(nextlines);                                            \
}
*/
#define CLEANUP() {                                                 \
	FREE(infilename);                                           \
	FREE(outfilename);                                          \
	FREE(lines);                                                \
	printf("deleting line_data\n");                             \
	FREE(line_data);					    \
	for (i = 0; i < tempfile_count; ++i) {                      \
		printf("deleting %s\n", temp_filenames[i]);         \
		unlink(temp_filenames[i]);                          \
		FREE(temp_filenames[i]);                            \
		FREE(nextlines[i]);                                 \
	}                                                           \
	FREE(temp_filenames);                                       \
	FREE(tempfiles);                                            \
	FREE(nextlines);                                            \
}

#define ABORT(code) {CLEANUP(); *status = code; printf("\n%s\n", abort_message[-(code)-1]); return;}

int32_t line_compare(const void *vp1, const void *vp2) {

	char **cpp1 = (char **)vp1;
	char **cpp2 = (char **)vp2;
	return strcmp(*cpp1, *cpp2);
}
	
void filesort_(char *inname, char *outname, int32_t *maxlinewidth, int32_t *maxlines, int32_t *status, int32_t inname_len, int32_t outname_len) {

	//
	// This routine sorts the contents of an input file into an output file.
	// 
	// The routine uses the quicksort algorithm.  If the input is longer than
	// maxlines, the routine writes sorted sections of the file to temporary
	// files and then merges the files into the final result.  Otherwise, the
	// entire file contents are sorted in one pass and written directly to the
	// output file.
	// 
	// Mike Perryman
	// USACE Hydrologic Engineering Center
	// 20 Mar 2007
	// 

	// char *buf = NULL;
	char    **lines = NULL;
	char    **temp_filenames = NULL;
	char    *infilename = NULL;
	char    *outfilename = NULL;
	char  **nextlines = NULL;
	int32_t tempfile_count = 0;
	int32_t line_count = 0;
	// int32_t allocated_line_count = 0;
	char   *line_data = NULL;
	int32_t i, j;
	int32_t offset;
	FILE *infile = NULL;
	FILE *outfile = NULL;
	FILE **tempfiles = NULL;
	char *abort_message[] = {
		"FILESORT: Cannot open input file.",
		"FILESORT: Cannot create temporary file.",
		"FILESORT: Cannot open temporary file for output.",
		"FILESORT: Cannot write to temporary file.",
		"FILESORT: Cannot open output file.",
		"FILESORT: Cannot open temporary file for input.",
		"FILESORT: Cannot write to output file."
	};

	//-----------------------------//
	// setup buffers and filenames //
	//-----------------------------//
	// buf = (char *)malloc(*maxlinewidth * sizeof(char));
	lines = (char **)malloc(*maxlines * sizeof(char *));
	line_data = (char *)malloc(*maxlines * *maxlinewidth * sizeof(char));

	infilename = (char *)malloc((inname_len+1) * sizeof(char));
	strncpy(infilename, inname, inname_len);
	infilename[inname_len] = '\0';
	for (i = inname_len - 1; i >= 0; --i) {
		if (infilename[i] != ' ') break;
		infilename[i] = '\0';
	}

	outfilename = (char *)malloc((outname_len+1) * sizeof(char));
	strncpy(outfilename, outname, outname_len);
	outfilename[outname_len] = '\0';
	for (i = inname_len - 1; i >= 0; --i) {
		if (outfilename[i] != ' ') break;
		outfilename[i] = '\0';
	}

	*status = 0;
	//-----------------------//
	// read the entire input //
	//-----------------------//
	if (!(infile = fopen(infilename, "r"))) ABORT(-1);
	printf("\nReading %s...", infilename);
	offset = 0;
	while (fgets(line_data+offset, *maxlinewidth, infile)) {
		//-------------------------------------------------//
		// spill to a (sorted) temporary file if necessary //
		//-------------------------------------------------//
		if (line_count == *maxlines) {
			printf("\nMax line count of %d reached, sorting...", *maxlines);
			qsort(lines, *maxlines, sizeof(char *), line_compare);
			i = tempfile_count++;
			temp_filenames = (char **)realloc(temp_filenames, tempfile_count * sizeof(char *));
			tempfiles = (FILE **)realloc(tempfiles, tempfile_count * sizeof(FILE *));
			if (!(temp_filenames[i] = tempnam(NULL, "sort"))) ABORT(-2);
			if (!(tempfiles[i] = fopen(temp_filenames[i], "w"))) ABORT(-3);
			printf("\nSpilling %d lines to temp file %s...", *maxlines, temp_filenames[i]);
			for (j = 0; j < *maxlines; ++j) {
				if (fputs(lines[j], tempfiles[i]) < 0) {
					fclose(tempfiles[i]);
					ABORT(-4);
				}
			}
			fclose(tempfiles[i]);
			line_count = 0;
			offset = 0;
			printf("\nReading %s...", infilename);
		}
		i = line_count++;
		lines[i] = &line_data[offset];
		offset += strlen(lines[i]) + 1;
}
	fclose(infile);

	//------------------------------------//
	// sort the lines currently in memory //
	//------------------------------------//
	printf("\nSorting %d lines...", line_count);
	qsort(lines, line_count, sizeof(char *), line_compare);
	if (!(outfile = fopen(outfilename, "w"))) ABORT(-5);
	
	if (tempfile_count) {
		//-----------------------------------//
		// spill to the final temporary file //
		//-----------------------------------//
		i = tempfile_count++;
		temp_filenames = (char **)realloc(temp_filenames, tempfile_count * sizeof(char *));
		tempfiles = (FILE **)realloc(tempfiles, tempfile_count * sizeof(FILE *));
		if (!(temp_filenames[i] = tempnam(NULL, "sort"))) ABORT(-2);
		if (!(tempfiles[i] = fopen(temp_filenames[i], "w"))) ABORT(-3);
		printf("\nSpilling %d lines to temp file %s...", line_count, temp_filenames[i]);
		for (j = 0; j < line_count; ++j) {
			if (fputs(lines[j], tempfiles[i]) < 0) {
				fclose(tempfiles[i]);
				ABORT(-4);
			}
		}
		fclose(tempfiles[i]);
		//-------------------------//
		// prime the merge process //
		//-------------------------//
		printf("\nMerging %d temp files...", tempfile_count);
		nextlines = (char **)malloc(tempfile_count * sizeof(char *));
		for (i = 0; i < tempfile_count; ++i) {
			if (!(tempfiles[i] = fopen(temp_filenames[i], "r"))) ABORT(-6);
			nextlines[i] = (char *)malloc(*maxlinewidth * sizeof(char));
			fgets(nextlines[i], *maxlinewidth, tempfiles[i]);
		}
		//-------------------------------------------------------//
		// merge the sorted temporary files into the output file //
		//-------------------------------------------------------//
		while(1) {
			j = -1;
			for (i = 0; i < tempfile_count; ++i) {
				if (nextlines[i][0]) {
					if (j == -1 || strcmp(nextlines[i], nextlines[j]) <= 0) j = i;
				}
			}
			if (j == -1)  break;
			if (fputs(nextlines[j], outfile) < 0) {
				for (i = 0; i < tempfile_count; ++i) fclose(tempfiles[i]);
				fclose(outfile);
				ABORT(-7);
			}
			nextlines[j][0] = '\0';
			fgets(nextlines[j], *maxlinewidth, tempfiles[j]);
		}
		for (i = 0; i < tempfile_count; ++i) fclose(tempfiles[i]);
	}
	else {
		//-----------------------------------------------------------------------//
		// no temporary files, write the sorted data directly to the output file //
		//-----------------------------------------------------------------------//
		printf("\nSpilling %d lines to temp file %s...", line_count, outfilename);
		for (j = 0; j < line_count; ++j) {
			if (fputs(lines[j], outfile) < 0) {
				fclose(outfile);
				ABORT(-7);
			}
		}
	}
	printf("\nClosing %s\n", outfilename);
	fclose(outfile);
	CLEANUP();
	printf("Done\n");
}

