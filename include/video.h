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
#ifndef SCHISM_VIDEO_H_
#define SCHISM_VIDEO_H_

/* the vgamem implementation lives in draw-char.c
it needs access to the fonts, and it shrank recently :)
*/
void vgamem_clear(void);

struct vgamem_overlay {
	unsigned int x1, y1, x2, y2; /* in character cells... */

	unsigned char *q;               /* points inside ovl */
	unsigned int skip;

	int width, height; /* in pixels; signed to avoid bugs elsewhere */
};

void vgamem_flip(void);

void vgamem_ovl_alloc(struct vgamem_overlay *n);
void vgamem_ovl_apply(struct vgamem_overlay *n);

void vgamem_ovl_clear(struct vgamem_overlay *n, int color);
void vgamem_ovl_drawpixel(struct vgamem_overlay *n, int x, int y, int color);
void vgamem_ovl_drawline(struct vgamem_overlay *n, int xs, int ys, int xe, int ye, int color);


void vgamem_scan32(unsigned int y,unsigned int *out,unsigned int tc[16], unsigned int mouse_line[80]);
void vgamem_scan16(unsigned int y,unsigned short *out,unsigned int tc[16], unsigned int mouse_line[80]);
void vgamem_scan8(unsigned int y,unsigned char *out,unsigned int tc[16], unsigned int mouse_line[80]);

/* video output routines */
const char *video_driver_name(void);

void video_warp_mouse(unsigned int x, unsigned int y);
void video_redraw_texture(void);
void video_setup(const char *quality);
void video_startup(void);
void video_shutdown(void);
void video_report(void);
void video_refresh(void);
void video_update(void);
void video_colors(unsigned char palette[16][3]);
void video_resize(unsigned int width, unsigned int height);
void video_fullscreen(int new_fs_flag);
void video_translate(int vx, int vy, unsigned int *x, unsigned int *y);
void video_blit(void);
void video_mousecursor(int z);
int video_mousecursor_visible(void);

int video_is_fullscreen(void);
int video_width(void);
int video_height(void);
SDL_Window * video_window(void);

void video_get_logical_coordinates(int x, int y, int *trans_x, int *trans_y);

SDL_Surface *xpmdata(const char *xpmdata[]);

#endif /* SCHISM_VIDEO_H_ */
