/*
 * Schism Tracker - a cross-platform Impulse Tracker clone
 * copyright (c) 2003-2005 Storlek <storlek@rigelseven.com>
 * copyright (c) 2005-2008 Mrs. Brisby <mrs.brisby@nimh.org>
 * copyright (c) 2009 Storlek & Mrs. Brisby
 * copyright (c) 2010-2012 Storlek
 * URL: http://schismtracker.org/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "headers.h"
#include "bswap.h"
#include "slurp.h"
#include "fmt.h"
#include "mem.h"
#include "version.h"
#include "log.h"

#include "player/sndfile.h"


static int stm_import_edittime(song_t *song, uint16_t trkvers, uint32_t reserved32);

/* --------------------------------------------------------------------- */

int fmt_stm_read_info(dmoz_file_t *file, slurp_t *fp)
{
	unsigned char title[20];
	int what, type, version;
	int i;

	slurp_seek(fp, 28, SEEK_SET);
	what = slurp_getc(fp);
	type = slurp_getc(fp);
	version = slurp_getc(fp);
	
	/* data[29] is the type: 1 = song, 2 = module (with samples) */
	if ((what != 0x1a && what != 0x02) || (type != 1 && type != 2)
		|| version != 2)
		return 0;

	slurp_seek(fp, 20, SEEK_SET);
	for (i = 0; i < 8; i++) {
		/* the ID should be all safe ASCII */
		int id = slurp_getc(fp);
		if (id < 0x20 || id > 0x7E)
			return 0;
	}

	slurp_rewind(fp);
	if (slurp_read(fp, title, sizeof(title)) != sizeof(title))
		return 0;

	/* I used to check whether it was a 'song' or 'module' and set the description
	 * accordingly, but it's fairly pointless information :) */
	file->description = "Scream Tracker 2";
	/*file->extension = str_dup("stm");*/
	file->type = TYPE_MODULE_MOD;
	file->title = strn_dup((const char *)title, sizeof(title));
	return 1;
}

/* --------------------------------------------------------------------- */

struct stm_sample {
	char name[12];
	uint16_t pcmpara; // in the official documentation, this is misleadingly labelled reserved...
	uint16_t length, loop_start, loop_end;
	uint8_t volume;
	uint16_t c5speed;
};

static int read_stm_sample(struct stm_sample *smp, slurp_t *fp)
{
#define READ_VALUE(name) \
	if (slurp_read(fp, &smp->name, sizeof(smp->name)) != sizeof(smp->name)) return 0

	READ_VALUE(name);
	slurp_seek(fp, 1, SEEK_CUR); // zero
	slurp_seek(fp, 1, SEEK_CUR); // inst_disk
	READ_VALUE(pcmpara);
	READ_VALUE(length);
	READ_VALUE(loop_start);
	READ_VALUE(loop_end);
	READ_VALUE(volume);
	slurp_seek(fp, 1, SEEK_CUR); // reserved
	READ_VALUE(c5speed);
	slurp_seek(fp, 4, SEEK_CUR); // morejunk
	slurp_seek(fp, 2, SEEK_CUR); // paragraphs (? what)

#undef READ_VALUE

	return 1;
}

/* ST2 says at startup:
"Remark: the user ID is encoded in over ten places around the file!"
I wonder if this is interesting at all. */
static void load_stm_pattern(song_note_t *note, slurp_t *fp)
{
	int row, chan;
	uint8_t v[4];

	for (row = 0; row < 64; row++, note += 64 - 4) {
		for (chan = 0; chan < 4; chan++) {
			song_note_t* chan_note = note + chan;
			slurp_read(fp, v, 4);

			// mostly copied from modplug...
			if (v[0] < 251)
				chan_note->note = (v[0] >> 4) * 12 + (v[0] & 0xf) + 37;
			chan_note->instrument = v[1] >> 3;
			if (chan_note->instrument > 31)
				chan_note->instrument = 0; // oops never mind, that was crap
			chan_note->volparam = (v[1] & 0x7) + ((v[2] & 0xf0) >> 1);
			if (chan_note->volparam <= 64)
				chan_note->voleffect = VOLFX_VOLUME;
			else
				chan_note->volparam = 0;
			chan_note->param = v[3]; // easy!

			chan_note->effect = stm_effects[v[2] & 0x0f];
			handle_stm_effects(chan_note);
		}

		for (chan = 0; chan < 4; chan++) {
			song_note_t* chan_note = note + chan;
			if (chan_note->effect == FX_SPEED) {
				uint32_t tempo = chan_note->param;
				chan_note->param >>= 4;
				handle_stm_tempo_pattern(note, tempo);
			}
		}

		note += chan;
	}
}

int fmt_stm_load_song(song_t *song, slurp_t *fp, unsigned int lflags)
{
	char id[8];
	uint8_t tmp[4];
	int npat, n;
	uint16_t para_sdata[MAX_SAMPLES] = { 0 };

	slurp_seek(fp, 20, SEEK_SET);
	slurp_read(fp, id, 8);
	slurp_read(fp, tmp, 4);

	if (!(
		// this byte is *usually* guaranteed to be 0x1a,
		// however putup10.stm and putup11.stm are outliers
		// for some reason?...
		(tmp[0] == 0x1a || tmp[0] == 0x02)
		// from the doc:
		//      1 - song (contains no samples)
		//      2 - module (contains samples)
		// I'm not going to care about "songs".
		&& tmp[1] == 2
		// do version 1 STM's even exist?...
		&& tmp[2] == 2
	)) {
		return LOAD_UNSUPPORTED;
	}
	// check the file tag for printable ASCII
	for (n = 0; n < 8; n++)
		if (id[n] < 0x20 || id[n] > 0x7E)
			return LOAD_FORMAT_ERROR;

	// and the next two bytes are the tracker version.
	if (!memcmp(id, "!Scream!", 8))
		// Unfortunately tools never differentiated themselves...
		if (tmp[3] > 20)
			// Future Crew chose to never increase their version numbers after 2.21 it seems!
			sprintf(song->tracker_id, "Scream Tracker 2.2+ or compatible");
		else
			sprintf(song->tracker_id, "Scream Tracker %1d.%02d or compatible", CLAMP(tmp[2], 0, 9), CLAMP(tmp[3], 0, 99));
	else if (!memcmp(id, "BMOD2STM", 8))
		sprintf(song->tracker_id, "BMOD2STM");
	else if (!memcmp(id, "WUZAMOD!", 8))
		sprintf(song->tracker_id, "Wuzamod"); // once a MOD always a MOD
	else if (!memcmp(id, "SWavePro", 8))
		sprintf(song->tracker_id, "SoundWave Pro %1d.%02d", CLAMP(tmp[2], 0, 9), CLAMP(tmp[3], 0, 99));
	else if (!memcmp(id, "!Scrvrt!", 8))
		sprintf(song->tracker_id, "Screamverter");
	else if (!memcmp(id, "!JadeST!", 8)) {
		slurp_seek(fp, 35, SEEK_SET);
		uint16_t trkvers, msglen;
		uint32_t trkvers2, edittime;
		slurp_read(fp, &trkvers, 2);
		trkvers = bswapLE16(trkvers);
		slurp_read(fp, &trkvers2, 4);
		trkvers2 = bswapLE32(trkvers2);
		slurp_read(fp, &edittime, 4);
		edittime = bswapLE32(edittime);
		slurp_read(fp, &msglen, 2);
		msglen = bswapLE16(msglen);
		sprintf(song->tracker_id, "Schism Tracker (RM fork) ");
		ver_decode_cwtv(trkvers, trkvers2, song->tracker_id + strlen(song->tracker_id));
		stm_import_edittime(song, 0x0000, trkvers2);
		if (msglen != 0) {
			slurp_seek(fp, -msglen, SEEK_END);
			slurp_read(fp, song->message, msglen);
			song->message[msglen] = '\0';
		}
	}
	else
		sprintf(song->tracker_id, "Unknown");

	slurp_seek(fp, 0, SEEK_SET);
	slurp_read(fp, song->title, 20);
	song->title[20] = '\0';
	slurp_seek(fp, 12, SEEK_CUR); // skip the tag and stuff

	size_t tempo = slurp_getc(fp);

	if (tmp[3] < 21) {
		tempo = ((tempo / 10) << 4) + tempo % 10;
	}

	song->initial_speed = (tempo >> 4) ? (tempo >> 4) : 1;
	song->initial_tempo = convert_stm_tempo_to_bpm(tempo);

	npat = slurp_getc(fp);
	song->initial_global_volume = 2 * slurp_getc(fp);
	slurp_seek(fp, 13, SEEK_CUR); // junk

	if (npat > 64)
		return LOAD_FORMAT_ERROR;

	for (n = 1; n <= 31; n++) {
		struct stm_sample stmsmp;
		uint16_t blen;
		song_sample_t *sample = song->samples + n;

		if (!read_stm_sample(&stmsmp, fp))
			return LOAD_FORMAT_ERROR;

		for (int i = 0; i < 12; i++) {
			if ((uint8_t)stmsmp.name[i] == 0xFF)
				stmsmp.name[i] = 0x20;
		}
		// the strncpy here is intentional -- ST2 doesn't show the '3' after the \0 bytes in the first
		// sample of pm_fract.stm, for example
		strncpy(sample->filename, stmsmp.name, 12);
		memcpy(sample->name, sample->filename, 12);
		blen = sample->length = bswapLE16(stmsmp.length);
		sample->loop_start = bswapLE16(stmsmp.loop_start);
		sample->loop_end = bswapLE16(stmsmp.loop_end);
		sample->c5speed = bswapLE16(stmsmp.c5speed);
		sample->volume = stmsmp.volume * 4; //mphack
		if (sample->loop_start < blen
			&& sample->loop_end != 0xffff
			&& sample->loop_start < sample->loop_end) {
			sample->flags |= CHN_LOOP;
			sample->loop_end = CLAMP(sample->loop_end, sample->loop_start, blen);
		}
		para_sdata[n] = bswapLE16(stmsmp.pcmpara);
	}

	int orderlist_size = tmp[3] ? 128 : 64;

	slurp_read(fp, song->orderlist, orderlist_size);
	for (n = 0; n < orderlist_size; n++) {
		if (song->orderlist[n] >= 64)
			song->orderlist[n] = ORDER_LAST;
	}

	if (lflags & LOAD_NOPATTERNS) {
		slurp_seek(fp, npat * 64 * 4 * 4, SEEK_CUR);
	} else {
		for (n = 0; n < npat; n++) {
			song->patterns[n] = csf_allocate_pattern(64);
			song->pattern_size[n] = song->pattern_alloc_size[n] = 64;
			load_stm_pattern(song->patterns[n], fp);
		}
	}

	if (!(lflags & LOAD_NOSAMPLES)) {
		for (n = 1; n <= 31; n++) {
			song_sample_t *sample = song->samples + n;

			if (sample->length < 3) {
				// Garbage?
				sample->length = 0;
			} else {
				slurp_seek(fp, para_sdata[n] << 4, SEEK_SET);
				csf_read_sample(sample, SF_LE | SF_PCMS | SF_8 | SF_M, fp);
			}
		}
	}

	for (n = 0; n < 4; n++)
		song->channels[n].panning = ((n & 1) ? 64 : 0) * 4; //mphack
	for (; n < 64; n++)
		song->channels[n].flags |= CHN_MUTE;
	song->pan_separation = 64;
	song->flags = SONG_ITOLDEFFECTS | SONG_COMPATGXX | SONG_NOSTEREO;

	return LOAD_SUCCESS;
}


/* --------------------------------------------------------------------- */

static int stm_import_edittime(song_t *song, uint16_t trkvers, uint32_t reserved32)
{
	if (song->histlen)
		return 0; // ?

	song->histlen = 1;
	song->history = mem_calloc(1, sizeof(*song->history));

	uint32_t runtime = it_decode_edit_timer(trkvers, reserved32);
	song->history[0].runtime = dos_time_to_ms(runtime);

	return 1;
}

enum {
	WARN_LINEARSLIDES,
	WARN_SAMPLEVOL,
	WARN_LOOPS,
	WARN_SAMPLEVIB,
	WARN_INSTRUMENTS,
	WARN_PATTERNLEN,
	WARN_MAXCHANNELS,
	WARN_NOTERANGE,
	WARN_EFFECTS,
	WARN_VOLEFFECTS,
	WARN_MAXSAMPLES,
	WARN_LONGSAMPLES,
	WARN_MAXPATS,
	WARN_BREAKOTHERROWS,
	WARN_FINESLIDES,
	WARN_EFFECTMEMORY,

	MAX_WARN
};

static const char *stm_warnings[] = {
	[WARN_LINEARSLIDES] = "Linear slides",
	[WARN_SAMPLEVOL]    = "Sample volumes",
	[WARN_LOOPS]        = "Sustain and Ping Pong loops",
	[WARN_SAMPLEVIB]    = "Sample vibrato",
	[WARN_INSTRUMENTS]  = "Instrument functions",
	[WARN_PATTERNLEN]   = "Pattern lengths other than 64 rows",
	[WARN_MAXCHANNELS]  = "Data outside 4 channels",
	[WARN_NOTERANGE]    = "Notes outside the range C-4 to B-6",
	[WARN_EFFECTS]      = "Effects K - Z",
	[WARN_VOLEFFECTS]   = "Extended volume column effects",
	[WARN_MAXSAMPLES]   = "Over 31 samples",
	[WARN_LONGSAMPLES]  = "sample length greater than 65535",
	[WARN_MAXPATS]      = "Over 63 patterns",
	[WARN_BREAKOTHERROWS]="Breaking to any other rows besides 0",
	[WARN_FINESLIDES]   = "(Extra-)Fine volume/portamento slides",
	[WARN_EFFECTMEMORY] = "Effect memory",

	[MAX_WARN]          = NULL
};

static int write_stm_sample(struct stm_sample *hdr, disko_t *fp)
{
#define WRITE_VALUE(x) do { disko_write(fp, &hdr->x, sizeof(hdr->x)); } while (0)

	WRITE_VALUE(name);
	disko_seek(fp, 1, SEEK_CUR); // zero
	disko_seek(fp, 1, SEEK_CUR); // inst_disk
	WRITE_VALUE(pcmpara);
	WRITE_VALUE(length);
	WRITE_VALUE(loop_start);
	WRITE_VALUE(loop_end);
	WRITE_VALUE(volume);
	disko_seek(fp, 1, SEEK_CUR); // reserved
	WRITE_VALUE(c5speed);
	disko_seek(fp, 4, SEEK_CUR); // morejunk
	disko_seek(fp, 2, SEEK_CUR); // paragraphs (? what)

#undef WRITE_VALUE

	return 1;
}

// RM: directly taken from Screamverter
static uint8_t calculate_tempo_scale(uint8_t speed, uint8_t tempo)
{
	unsigned int calculated_tempo;
	unsigned int original_tempo;
	signed int tempo_difference = 0;
	signed int speed_difference = 0;

	if (speed == 0 || tempo == 0) {
		speed = 6;
		tempo = 125;
	}
	speed = MIN(15, speed);
	original_tempo = (tempo * 6) / speed;
	for (int i = 15; i > 0; --i) {
		for (int j = 0; j < 16; ++j) {
		calculated_tempo = (st2_tempo_table[i - 1][j] * 6) / i;
		tempo_difference = original_tempo - calculated_tempo;
		speed_difference = speed - i;

		if (calculated_tempo == original_tempo ||
			((tempo_difference >= -2 && tempo_difference <= 2) &&
			(speed_difference >= -1 && speed_difference <= 1))) 
			return (i << 4) | j;
		}
	}

	return tempo;
}

int fmt_stm_save_song(disko_t *fp, song_t *song)
{
	char stm_songtitle[20];
	uint8_t stm_orders[128];
	uint8_t tmp[128];
	uint32_t para_sdata[MAX_SAMPLES];
	uint8_t speed = song->initial_speed;
	uint8_t tempo = song->initial_tempo;

	int nord, nsmp, npat, maxpat, jmax, joutpos;
	int i, j, n;
	unsigned int warn = 0;

	if (song->flags & SONG_INSTRUMENTMODE)
		warn |= 1 << WARN_INSTRUMENTS;
	if (song->flags & SONG_LINEARSLIDES)
		warn |= 1 << WARN_LINEARSLIDES;
	npat = csf_get_num_patterns(song);
	if (npat > 63) {
		npat = 63;
		warn |= 1 << WARN_MAXPATS;
	}
	nsmp = csf_get_num_samples(song); // Getting number of samples
	if (nsmp > 31) {
		nsmp = 31;
		warn |= 1 << WARN_MAXSAMPLES;
	}
	nord = csf_get_num_orders(song); // or "csf_get_num_orders(song_t *csf);"
	if (3 < csf_get_highest_used_channel(song))
		warn |= 1 << WARN_MAXCHANNELS;

	log_appendf(5, " %d orders, %d samples, %d patterns", nord, nsmp, npat);

	strncpy(stm_songtitle, song->title, 20);
	disko_write(fp, stm_songtitle, 20); // writing song title
	disko_write(fp, "!JadeST!\x1a\x02\x02\x15", 12);
	disko_putc(fp, calculate_tempo_scale(speed, tempo));
	disko_putc(fp, npat);
	disko_putc(fp, song->initial_global_volume / 2);
	{
		uint16_t cwtv = 0x4000 | ver_cwtv;
		uint32_t cwtv2 = ver_reserved;
		uint32_t time = 0;
		uint16_t msglen = strlen(song->message);
		for (size_t t = 0; t < song->histlen; t++)
			time += ms_to_dos_time(song->history[t].runtime);

		// 32-bit DOS tick count (tick = 1/18.2 second; 54945 * 18.2 = 999999 which is Close Enough)
		time += it_get_song_elapsed_dos_time(song);

		tmp[0] = cwtv & 0xff;
		tmp[1] = cwtv >> 8;
		tmp[2] = cwtv2 & 0xff;
		tmp[3] = cwtv2 >> 8;
		tmp[4] = cwtv2 >> 16;
		tmp[5] = cwtv2 >> 24;
		tmp[6] = time & 0xff;
		tmp[7] = time >> 8;
		tmp[8] = time >> 16;
		tmp[9] = time >> 24;
		tmp[10] = msglen & 0xff;
		tmp[11] = msglen >> 8;
		tmp[12] = 'J';
	}
	disko_write(fp, tmp, 13);
	disko_seek(fp, 992, SEEK_CUR); // deal with sample headers later
	memset(stm_orders, 99, 128);
	for(n = i = 0; (i < nord) && (i < 128); ++i) {
		if (song->orderlist[i] == ORDER_SKIP || song->orderlist[i] == ORDER_LAST)
			continue;
		stm_orders[n++] = song->orderlist[i];
	}
	disko_write(fp, stm_orders, 128);

	for(n = 0; n < npat; ++n) {
		song_note_t *m = song->patterns[n];
		uint8_t saved_param[4] = {0};
		if (m == NULL) continue;	
		jmax = song->pattern_size[n];
		if(jmax != 64) {
			if(jmax > 64)
				jmax = 64;
			warn |= 1 << WARN_PATTERNLEN;
		}
		jmax *= MAX_CHANNELS;
		for (j = 0; j < jmax; ++j, ++m) {
			if ((j % MAX_CHANNELS) < 4) {
				int check_effect_memory = 0;
				int speed_andor_tempo_specified = 0;
				song_note_t out = *m;
				uint8_t stm_fx = 0, stm_fx_val = out.param, stm_vol = out.voleffect == VOLFX_VOLUME ? out.volparam : 65;

				if (out.param)
					saved_param[j % MAX_CHANNELS] = m->param;

				if ((out.note > 0 && out.note <= 12) || (out.note >= 109 && out.note <= 120)) {
					warn |= 1 << WARN_NOTERANGE;
					out.note = 255;
				} else if (out.note > 12 && out.note < 109) {
					out.note -= 13;
					out.note = (out.note % 12) + (((out.note / 12) - 2) << 4);
				} else if (out.note == NOTE_CUT || out.note == NOTE_OFF) {
					out.note = 254;
				} else {
					out.note = 255;
				}

				switch(out.effect) {
					case FX_NONE: stm_fx = stm_fx_val = 0; break;
					case FX_SPEED:
						speed = out.param;	
						speed_andor_tempo_specified = 1;
						break;
					case FX_TEMPO:
						tempo = out.param;
						speed_andor_tempo_specified = 1;
						break;
					case FX_ARPEGGIO: stm_fx = 0x0a; break;
					case FX_POSITIONJUMP: stm_fx = 0x02; break;
					case FX_PATTERNBREAK:
						stm_fx = 0x03;
						if (stm_fx_val > 0) {
							warn |= 1 << WARN_BREAKOTHERROWS;
						}
						break;
					case FX_VOLUMESLIDE:
						stm_fx = 0x04;
						if (((stm_fx_val & 0x0f) >= 1 && (stm_fx_val >> 4) == 0x0f) ||
						((stm_fx_val & 0x0f) == 0x0f && (stm_fx_val >> 4) >= 1)) {
							const uint8_t l = stm_fx_val & 0x0f;
							const uint8_t h = stm_fx_val >> 4;
							warn |= 1 << WARN_FINESLIDES;
							if (l == 0xf)
								stm_fx_val = CLAMP(h / speed, 1, 0xF) << 4;
							else if (h == 0xf)
								stm_fx_val = CLAMP(l / speed, 1, 0xF);
						}
						else if ((stm_fx_val & 0x0f) && (stm_fx_val >> 4))
							stm_fx_val &= 0x0f;
						check_effect_memory = 1;
						break;
					case FX_PORTAMENTODOWN:
					case FX_PORTAMENTOUP:
						stm_fx = (out.effect == FX_PORTAMENTOUP) ? 0x06 : 0x05;
						
						if (stm_fx_val > 0xdf) {
							warn |= 1 << WARN_FINESLIDES;
							const int divisor = speed * (stm_fx_val >> 4) == 0xE ? 4 : 1;
							stm_fx_val = CLAMP((((stm_fx_val & 0xf) + (divisor - 1)) / (divisor)), 1, 0xF);	
						}
						stm_fx_val = MIN(0xdf,stm_fx_val);
						check_effect_memory = 1;
						break;
					case FX_TONEPORTAMENTO:
						stm_fx = 0x07;
						check_effect_memory = 1;
						break;
					case FX_VIBRATO:
						stm_fx = 0x08;
						if ((song->flags & SONG_ITOLDEFFECTS) == 0)
							stm_fx_val = (stm_fx_val & 0xf0) | CLAMP((stm_fx_val*2),1,0xF);
						check_effect_memory = 1;
						break;
					case FX_TREMOR: stm_fx = 0x09;
						break;
					default:
						warn |= 1 << WARN_EFFECTS;
						break;
				}

				if (check_effect_memory && !out.param)
					stm_fx_val = saved_param[j % MAX_CHANNELS];

				check_effect_memory = 0;

				if (!stm_fx) {
					switch (out.voleffect) {
					case VOLFX_NONE:
					case VOLFX_VOLUME:
						// ok
						break;
					case VOLFX_VOLSLIDEUP:
						stm_fx = 0x04;
						stm_fx_val = out.volparam << 4;
						check_effect_memory = 1;
						break;
					case VOLFX_VOLSLIDEDOWN:
						stm_fx = 0x04;
						stm_fx_val = out.volparam;
						check_effect_memory = 1;
						break;
					case VOLFX_PORTAUP:
						stm_fx = 0x06;
						stm_fx_val = vc_portamento_table[out.volparam & 0xf];
						check_effect_memory = 1;
						break;
					case VOLFX_PORTADOWN:
						stm_fx = 0x05;
						stm_fx_val = vc_portamento_table[out.volparam & 0xf];
						check_effect_memory = 1;
						break;
					case VOLFX_TONEPORTAMENTO:
						stm_fx = 0x3;
						stm_fx_val = vc_portamento_table[out.volparam & 0xf];
						check_effect_memory = 1;
						break;
					default:
						/* oh well */
						warn |= 1 << WARN_VOLEFFECTS;
						break;
					}
				}

				if (check_effect_memory && !m->volparam)
					stm_fx_val = saved_param[j % MAX_CHANNELS];

				if (!stm_fx && speed_andor_tempo_specified) {
					stm_fx = 0x01;
					stm_fx_val = calculate_tempo_scale(speed, tempo);
				}
#if 1
				disko_putc(fp, out.note);
				disko_putc(fp, (out.instrument << 3) | (stm_vol & 0x07));
				disko_putc(fp, ((stm_vol & 0x78) << 1) | (stm_fx & 0x0f));
				disko_putc(fp, stm_fx_val);		
#else
				if (out.note == 0xfe && !out.instrument && stm_vol >= 65 && !stm_fx)
					disko_putc(fp, 0xfd);
				else if (out.note == 0xff && !out.instrument && stm_vol >= 65 && !stm_fx)
					disko_putc(fp, 0xfc);
				else {
					disko_putc(fp, out.note);
					disko_putc(fp, (out.instrument << 3) | (stm_vol & 0x07));
					disko_putc(fp, ((stm_vol & 0x78) << 1) | (stm_fx & 0x0f));
					disko_putc(fp, stm_fx_val);			
				}
#endif
			}
		}
	}

	// Now writing sample data
	for (n = 0; (n < nsmp) && (n < 31); ++n) {
		song_sample_t *smp = song->samples + (n + 1);
		disko_align(fp, 16);
		para_sdata[n] = disko_tell(fp);	
		if (smp->data) {
			csf_write_sample(fp, smp, SF(PCMS,8,M,LE), UINT16_MAX);
		}
	}

	disko_write(fp, song->message, strlen(song->message));

	disko_seek(fp, 48, SEEK_SET);
	// Now writing sample headers
	for(n = 1; n <= 31; ++n) {
		song_sample_t *smp = &song->samples[n];
		struct stm_sample stmsmp;
		memset(&stmsmp, 0, sizeof(stmsmp));
		stmsmp.loop_end = 0xffff;
		if(n <= nsmp) {
			if(smp->global_volume != 64)
				warn |= 1 << WARN_SAMPLEVOL;
			if((smp->flags & (CHN_LOOP | CHN_PINGPONGLOOP)) == (CHN_LOOP | CHN_PINGPONGLOOP) || (smp->flags & CHN_SUSTAINLOOP))
				warn |= 1 << WARN_LOOPS;
			if(smp->vib_depth != 0)
				warn |= 1 << WARN_SAMPLEVIB;

			if (smp->filename[0] != '\0')
				strncpy((char*)stmsmp.name, smp->filename, 12);
			else {
					for (i = 11; i >= 0; i--)
						if ((smp->name[i] ? smp->name[i] : 32) != 32)
							break;
					for (; i >= 0; i--)
						stmsmp.name[i] = smp->name[i] ? smp->name[i] : 255;
			}

			stmsmp.length = MIN(0xffff,smp->length);
			if (smp->flags & CHN_LOOP) {
				stmsmp.loop_start = MIN(stmsmp.length, smp->loop_start);
				stmsmp.loop_end = MIN(stmsmp.length, smp->loop_end);
			} else {
				stmsmp.loop_start = 0;
				stmsmp.loop_end = 0xFFFF;
			}
			stmsmp.length = bswapLE16(stmsmp.length);
			stmsmp.loop_start = bswapLE16(stmsmp.loop_start);
			stmsmp.loop_end = bswapLE16(stmsmp.loop_end);
			stmsmp.c5speed = bswapLE16(smp->c5speed);
			stmsmp.volume = stmsmp.length == 0 ? 0 : (smp->volume + 1) / 4;
			stmsmp.pcmpara = bswapLE16(MIN(0xffff, para_sdata[n-1] >> 4));
		}
		write_stm_sample(&stmsmp, fp);
	}

	/* announce all the things we broke - ripped from s3m.c */
	for (n = 0; n < MAX_WARN; ++n) {
		if (warn & (1 << n))
			log_appendf(4, " Warning: %s unsupported in STM format", stm_warnings[n]);
	}

	return SAVE_SUCCESS;
}
