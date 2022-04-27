# -*- coding: utf-8 -*-
"""
massplot.py - for easier fast plotting in matplotlib

https://github.com/scantle/massplot
leland@scantle.com
"""
# Python 2/3 compatibility
from __future__ import print_function

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.patches as patches
from matplotlib import gridspec
from descartes import PolygonPatch

#--------------------------------------------------------------------------------------------------#

# For faster plotting...
class create(object):
    """
    Variables:
        fig
        ax
        axinset
        feature_list
        text_list
        colors
        marks
        legend_loc
        legend_size
        legend_ncol
        colors_used
        legend_mask
        current_loc

    Methods:
        __init__()
        add_features()
        update_feature()
        drop_features()
        create_legend()
        update_legend()
        set_title()
        create_minimap()
        minimap_current_loc()
        add_to_pdf()

    TODO: List current features method
    """

    def __init__(self, xlims=None, ylims=None, xlabel=None, ylabel=None, xscale='linear',
                 yscale='linear', figwidth=11, figheight=8.5, **kwargs):
        """
        Creates a single plot that can be easily and rapidly updated with new data. Plots can be
        output between updates, allowing for a "massive" amount of plots to be created from one
        single object.

        :param xlabel:
        :param ylabel:
        :param xscale:
        :param yscale:
        :param figwidth:
        :param figheight:
        Kwargs:
            xlabelsize
            ylabelsize
            fig
            sublot
        TODO: Have start figure / update visible methods that make user masking functions obsolete
        """

        self.feature_list = []
        self.text_list = []
        # Add a markers list
        self.colors = ['#4e79a7','#59a14f','#e15759','#76b7b2','#f28e2b','#edc948','#b07aa1',
                       '#ff9da7','#9c755f','#bab0ac','#76b7b2','#f28e2b','#edc948','#b07aa1',
                       '#ff9da7','#9c755f','#bab0ac','#76b7b2','#f28e2b','#edc948','#b07aa1',
                       '#ff9da7','#9c755f','#bab0ac','#76b7b2','#f28e2b','#edc948','#b07aa1',
                       '#ff9da7','#9c755f','#bab0ac']
        self.color_mask = [False for i in range(0,len(self.colors))]
        # TODO: Change to visible_mask, add map_visible_mask
        self.legend_mask = []

        # If passed a fig and a subplot ID, doesn't create a new plot - just creates a new
        # massplot on the existing feature
        if ('subplot' in kwargs) and ('fig' in kwargs):
            # TODO find out what are drawbacks/benefits of not setting self.fig
            self.ax = kwargs['fig'].add_subplot(kwargs['subplot'])
        else:
            self.fig = plt.figure(figsize=(figwidth, figheight))
            self.ax = self.fig.add_subplot(111)

        # Axis settings
        if xlabel is not None:
            if 'xlabelsize' in kwargs:
                xlabelsize = kwargs['xlabelsize']
            else:
                xlabelsize = 10
            self.set_xlabel(xlabel, fontsize=xlabelsize)
        if ylabel is not None:
            if 'ylabelsize' in kwargs:
                ylabelsize = kwargs['ylabelsize']
            else:
                ylabelsize = 10
            self.set_ylabel(ylabel, fontsize=ylabelsize)
        # Tick labels
        if 'ticklabelsize' in kwargs:
            ticklabelsize = kwargs['ticklabelsize']
        else:
            ticklabelsize = 12
        self.ax.tick_params(axis='both', labelsize=ticklabelsize)
        self.ax.xaxis.set_ticks_position('bottom')
        self.ax.yaxis.set_ticks_position('left')

        # Handle if xscale is date
        # TODO: REMOVE or REPLACE - does NOT work better than just using linear and passing a date
        if xscale.lower() == 'date':
            years = mdates.YearLocator()
            months = mdates.MonthLocator()
            #yearsFmt = mdates.DateFormatter('%Y')
            #self.ax.xaxis.set_major_locator(years)
            #self.ax.xaxis.set_major_formatter(yearsFmt)
            self.ax.xaxis.set_minor_locator(years)
        else:
            self.ax.set_xscale(xscale)
        self.ax.set_yscale(yscale)
        self.ax.tick_params(axis='x')
        if xscale == 'log':
            self.ax.xaxis.grid(True, which='minor')
        if yscale == 'log':
            self.ax.yaxis.grid(True, which='minor')
        if xlims is not None:
            self.set_xlim(xlims)
        if ylims is not None:
            self.set_ylim(ylims)

    def set_xlim(self, xlims, **kwargs):
        """
        Sets xlimits, wrapper for ax.set_xlim
        """
        self.ax.set_xlim(xlims, **kwargs)

    def set_ylim(self, ylims, **kwargs):
        """
        Sets ylimits, wrapper for ax.set_ylim
        """
        self.ax.set_ylim(ylims, **kwargs)

    def set_xlabel(self, xlabel, **kwargs):
        """
        Sets x-axis label, wrapper for ax.set_xlabel()
        """
        self.ax.set_xlabel(xlabel, **kwargs)

    def set_ylabel(self, ylabel, **kwargs):
        """
        Sets y-axis label, wrapper for ax.set_ylabel()
        """
        self.ax.set_ylabel(ylabel, **kwargs)

    def set_ticks_y(self, size, **kwargs):
        """
        Sets y-axis tick label properties, wrapper for ax.tick_params
        """
        self.ax.tick_params(axis='y', labelsize=size, **kwargs)

    def set_ticks_x(self, size, **kwargs):
        """
        Sets y-axis tick label properties, wrapper for ax.tick_params
        """
        self.ax.tick_params(axis='x', labelsize=size, **kwargs)

    def add_feature(self, style, color=None, label=None, inlegend=True, line=True, empty=False, clip_on=True, **kwargs):
        # TODO: Also add to map, with options to not show
        # Add to legend mask list
        self.legend_mask.append(inlegend)
        # Auto assign a color if none given
        if color is None:
            color = self._checkout_color()
        if label is None:
            label = 'New Feature'
        if line == False:
            # Basically to prevent auto-added non-detect features from drawing lines
            # TODO make kwarg
            style = style.replace('-','')
        # Add to list, add to plot (in legendable, otherwise)
        feature_index = len(self.feature_list)
        if not empty:
            self.feature_list.append(self.ax.plot([], [], style,
                                                  color=color, label=label, clip_on=clip_on, **kwargs)[0])
        if empty:
            # As in, no center
            self.feature_list.append(self.ax.plot([], [], style, color=color,
                                              mec=color, mfc='none', clip_on=clip_on,
                                              label=label, **kwargs)[0])
        # Report index for user
        print("New Feature Index: " + str(feature_index))

    def add_features_same_color(self, num_features, style, color=None, inlegend=True, **kwargs):
        # Auto assign a color if none given
        if color is None:
            color = self._checkout_color()
        for i in range(num_features):
            self.add_feature(style, color=color, inlegend=inlegend, **kwargs)

    def add_features(self, num_features, style, inlegend=True, **kwargs):
        for i in range(num_features):
            self.add_feature(style, inlegend=inlegend, **kwargs)

    # For chemheads
    def add_ND_pair_feature(self, style, color=None, **kwargs):
        if color is None:
            color = self._checkout_color()
        # One in the legend
        self.add_feature(style, color, inlegend=True, **kwargs)
        # The assumption is the user will not want ND value in the legend
        # They will instead use the add_legend_ND_feature method
        self.add_feature(style, color, inlegend=False, line=False, empty=True, **kwargs)

    # Even crazier
    def mass_add_chem(self, num_locs, num_analytes, symbols, nd=True, **kwargs):
        # Get colors
        color_list = []
        for i in range(num_analytes):
            color_list.append(self._checkout_color())
        for i in range(num_locs):
            print("Index " + str(i) + "--------------------")
            for j in range(num_analytes):
                print("Analyte " + str(j) + "----------")
                self.add_feature(symbols[i], color_list[j], inlegend=True, **kwargs)
                if nd:
                    self.add_feature(symbols[i], color_list[j],
                                     inlegend=False, line=False, empty=True, **kwargs)
        print("All done!")

    def add_legend_ND_feature(self, color=None, **kwargs):
        # TODO: Auto move to end of feature_list when legend is updated
        # TODO: Even better make this a proxy artist:
        # http://matplotlib.org/users/legend_guide.html
        if color is None:
            color = '#666666'
        self.legend_mask.append(True)
        feature_index = len(self.feature_list)
        self.feature_list.append(self.ax.plot([], [],
                                'o', mec=color, mfc='none',
                                label="Non-Detects", **kwargs)[0])
        print("ND Feature Index: " + str(feature_index))

    def update_feature(self, feature_num, x, y, label=None, inlegend=True, rasterized=None):
        x, y = self._strip_to_data([x, y])
        self.feature_list[feature_num].set_data(x, y)
        if label is not None:
            self.feature_list[feature_num].set_label(label)
        if inlegend is not None:
            self.legend_mask[feature_num] = inlegend
        if rasterized is not None:
            self.feature_list[feature_num].set_rasterized(rasterized)

    def mask_feature(self, feature_nums):
        if isinstance(feature_nums, list):
            for i in feature_nums:
                self.update_feature(i, [], [])
                self.legend_mask[i] = False
        if isinstance(feature_nums, int):
            self.update_feature(feature_nums, [], [])
            self.legend_mask[feature_nums] = False

    def remove_feature(self, feature_nums):
        if isinstance(feature_nums, list):
            for i in feature_nums:
                self._checkin_color(self.feature_list[i].get_color())
                del(self.feature_list[i])
                del(self.legend_mask[i])
        if isinstance(feature_nums, int):
            self._checkin_color(self.feature_list[feature_nums].get_color())
            del(self.feature_list[feature_nums])
            del(self.legend_mask[feature_nums])

    def add_rectangle(self, x_start, x_end, y_start, y_end, color='#696969', alpha=1, inlegend=True,
                      label=None, **kwargs):
        """ Creates a rectangle (patch) on the plot area.
        Keyword Arguments:
            x_start (float or datetime) :   Left x coordinate
            x_end (float or datetime) :     Right x coordinate
            y_start (float) :               Top y coordinate
            y_end (float) :                 Bottom y coordinate
            color (matplotlib color spec):  Face color of rectangle
            inlegend (bool) :               T/F if the rectangle should appear in the legend
            label (string) :                Label for legend
            alpha (float) :                 Alpha transparency
            **kwargs are passed to patches.Rectangle()
        """
        # See if we have date objects
        if isinstance(x_start, mdates.datetime.datetime):
            x_start = mdates.date2num(x_start)
            x_end = mdates.date2num(x_end)
        if isinstance(y_start, mdates.datetime.datetime):
            y_start = mdates.date2num(y_start)
            y_end = mdates.date2num(y_end)
        rec_patch = patches.Rectangle((x_start, y_start),
                                      x_end - x_start,
                                      y_end - y_start,
                                      alpha = alpha,
                                      color = color,
                                      ec = None,
                                      label = label, **kwargs)
        feature_index = len(self.feature_list)
        self.ax.add_patch(rec_patch)
        self.legend_mask.append(inlegend)
        self.feature_list.append(rec_patch)
        # Report index for user
        print("New Feature Index: " + str(feature_index))

    def update_rectangle(self, feature_num, x_start=None, x_end=None, y_start=None, y_end=None,
                         label=None):
        """ Updates position/size of an existing rectangle (patch) on the plot area.
        Keyword Arguments:
            feature_num :                   The index of the rectangle feature, as reported when
                                            it was created
            x_start (float or datetime) :   Left x coordinate
            x_end (float or datetime) :     Right x coordinate
            y_start (float) :               Top y coordinate
            y_end (float) :                 Bottom y coordinate
            label (string) :                Label for legend
        """
        if x_start is not None:
            if isinstance(x_start, mdates.datetime.datetime):
                x_start = mdates.date2num(x_start)
            self.feature_list[feature_num].set_x(x_start)

        if x_end is not None:
            if isinstance(x_end, mdates.datetime.datetime):
                x_end = mdates.date2num(x_start)
            if x_start is None:
                x_start = self.feature_list[feature_num].get_x()
            self.feature_list[feature_num].set_width(x_end - x_start)

        if y_start is not None:
            if isinstance(y_start, mdates.datetime.datetime):
                y_start = mdates.date2num(y_start)
            self.feature_list[feature_num].set_y(y_start)

        if y_end is not None:
            if isinstance(y_end, mdates.datetime.datetime):
                y_end = mdates.date2num(y_start)
            if y_start is None:
                y_start = self.feature_list[feature_num].get_y()
            self.feature_list[feature_num].set_height(y_end - y_start)

        if label is not None:
            self.feature_list[feature_num].set_label(label)

    def create_legend(self, loc, size, ncol, **kwargs):
        """ Creates a legend of features.
        Arguments:
            loc (str or int):    valid matplotlib legend location argument
            size (float or int): legend text size
            ncol (int):          number of columns in legend
            **kwargs passed to ax.legend()
        """
        self.legend_loc = loc
        self.legend_size = size
        self.update_legend(ncol=ncol, **kwargs)

    def update_legend(self, ncol=None, **kwargs):
        """
        """
        if ncol is None:
            ncol = self.legend_ncol
        else:
            self.legend_ncol = ncol
        legend_list = [self.feature_list[i] for i, val in enumerate(self.legend_mask) if val]
        labs = [feat.get_label() for feat in legend_list]
        self.ax.legend(legend_list, labs,
                       loc = self.legend_loc,
                       prop={'size':self.legend_size},
                       ncol = self.legend_ncol,
                       **kwargs)

    def set_title(self, title, fontsize = 12, **kwargs):
        # A very light wrapper
        self.ax.set_title(title, fontsize = fontsize, **kwargs)

    def add_text(self, x, y, text, datacoords=False, **kwargs):
        """ Adds text to the plot.
        Arguments:
            x (float): X location
            y (float): Y location
            text (str): text to be added
            datacoords (T/F): Whether the data or axes coordinates should be used
                              Defaults is axes coordinates (from 0 to 1)
            **kwargs is passed to ax.text()
        """
        if datacoords is False:
            kwargs['transform'] = self.ax.transAxes
        text_index = len(self.text_list)
        self.text_list.append(self.ax.text(x, y, s=text, **kwargs))
        # Report index for user
        print("New Text Index: " + str(text_index))

    def update_text(self, index, text, **kwargs):
        """ Updates plot text box
        Arguments:
            index (int): Index of text object to be updated
            text (str): New text to be displayed
            **kwargs passed to ax.text object update
        """
        kwargs['text'] = text
        self.text_list[index].update(kwargs)

    def remove_text(self, index):
        """ Removes text from plot (and from text index list)
        Arguments:
            index (int): Index of text object to be removed
        """
        target = self.text_list.pop(index)
        target.remove()

    def create_minimap(self, map_right, map_bottom, map_w, map_h, x, y, xbuffer,
                       ybuffer, xy_color, xy_size, shapelist=None, shapecolors=None,
                       linewidths=None, zorders=None, rasterized=True, **kwargs):
        x, y = self._strip_to_data([x, y])
        # right, bottom, w x h
        self.axinset = plt.axes([map_right, map_bottom, map_w, map_h], aspect='equal',**kwargs)
        self.axinset.grid(b=None)
        self.axinset.set_facecolor('white')
        if rasterized:
            self.axinset.set_rasterized(rasterized)
        if shapelist is not None:
            # Plot Shapefile(s)
            self.add_shapefiles(self.axinset, shapelist, shapecolors, linewidths, zorders)
        # Add coords
        self.axinset.plot(x, y, 'o', color = xy_color, ms = xy_size)
        self.axinset.set_ylim([min(y) - ybuffer, max(y) + ybuffer])
        self.axinset.set_xlim([min(x) - xbuffer, max(x) + xbuffer])
        # Add blank feature for current location
        self.current_loc = self.axinset.plot([], [], 'o')[0]
        # Remove all axes junk
        self._blankify_plot(self.axinset)

    def minimap_current_loc(self, x, y, xy_color, xy_size):
        # Todo: Handle "AttributeError" when map has not been created
        x, y = self._strip_to_data([x, y])
        self.current_loc.set_data(x, y)
        self.current_loc.set_markerfacecolor(xy_color)
        self.current_loc.set_markeredgecolor(xy_color)
        self.current_loc.set_markersize(xy_size)

    def refresh_axis_scale(self, axis):
        """ Refreshes the axes of the plot to reflect the current data

        Keyword arguments:
            axis (string) 'x', 'y', 'xy', or 'both'
        """
        # Handling of xy (matplotlib prefers the string 'both')
        if axis =='xy':
            axis = 'both'
        self.ax.relim()
        self.ax.autoscale(axis = axis)

    def autoscale_axis(self, axis, buffer=0, min_diff=0):
        """
        autoscale_axis
        TODO: Support 'both' axes
        """
        local_list = [self.feature_list[i] for i, val in enumerate(self.legend_mask) if val]
        # Find min/max of axis
        # TODO: Make X like Y
        if axis == 'x':
            new_min = min([item.get_xdata().min() for item in local_list]) - buffer
            new_max = max([item.get_xdata().max() for item in local_list]) + buffer
        if axis == 'y':
            new_min = pd.np.nanmin([pd.np.nanmin(item.get_ydata()) for item in local_list]) - buffer
            new_max = pd.np.nanmax([pd.np.nanmax(item.get_ydata()) for item in local_list]) + buffer
        if axis == 'both':
            raise NotImplementedError('autoscale_axis only supports individual axes, x or y.')
        # Enfore minimum difference
        while (new_max - new_min < min_diff):
            new_max += 1
            new_min -= 1
        if axis == 'x':
            self.ax.set_xlim((new_min, new_max))
        if axis == 'y':
            self.ax.set_ylim((new_min, new_max))

    def set_fig_area(self, rect, **kwargs):
        """
        Sets the padding for the figure. Wrapper for fig.tight_layout()
        """
        self.fig.tight_layout(rect=rect, **kwargs)

    @staticmethod
    def add_to_pdf(pdf_object, **kwargs):
        pdf_object.savefig(**kwargs)

    # Helper "Private" Functions

    # Remove these, replace with a manager that cycles colors & markers
    def _checkout_color(self):
        color_index = self.color_mask.index(False)
        color = self.colors[color_index]
        self.color_mask[color_index] = True
        return color

    def _checkin_color(self, returned_color):
        # Is it even one of our colors?
        if returned_color in self.colors:
            color_index = self.colors.index(returned_color)
            self.color_mask[color_index] = False

    @staticmethod
    def _strip_to_data(items):
        for i, obj in enumerate(items):
            if isinstance(obj, pd.Series):
                items[i] = obj.values
        return items

    @staticmethod
    def _blankify_plot(axis_object):
        axis_object.tick_params(axis='x',          # changes apply to the x-axis
            which='both',      # both major and minor ticks are affected
            bottom=False,      # ticks along the bottom edge are off
            top=False,         # ticks along the top edge are off
            labeltop=False,
            labelbottom=False) # labels along the bottom edge are off
        axis_object.tick_params(axis='y',          # changes apply to the x-axis
            which='both',      # both major and minor ticks are affected
            left=False,      # ticks along the bottom edge are off
            right=False,         # ticks along the top edge are off
            labelright=False,
            labelleft=False) # labels along the bottom edge are off
        axis_object.xaxis.set_ticklabels([])
        axis_object.yaxis.set_ticklabels([])

    # "Public" shapefile functions
    @staticmethod
    def getShapeType(shapeobj):
        # Source: pg4 of
        #https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf
        if shapeobj.shapeType in [1,11,21]:
            return 'point'
        if shapeobj.shapeType in [3,13,23]:
            return 'line'
        if shapeobj.shapeType in [5,15,25]:
            return 'polygon'
        if shapeobj.shapeType in [8,18,28]:
            return 'multipoint'
        if shapeobj.shapeType in [31]:
            return 'multipatch'

    def add_shapefiles(self, axis_object, shapelist, shapecolors, linewidths=None, zorders=None,
                       **kwargs):
        if linewidths is None:
            linewidths = [1] * len(shapecolors)
        if zorders is None:
            zorders = [1] * len(shapecolors)
        for i, item in enumerate(shapelist):
            item_type = self.getShapeType(item)
            if item_type=='polygon':
                for shape in item.iterShapes():
                    axis_object.add_patch(PolygonPatch(shape,
                                                       fc=shapecolors[i],
                                                       ec=shapecolors[i],
                                                       zorder=zorders[i]),
                                          **kwargs)
            if item_type=='line':
                for shape in item.iterShapes():
                    x = [j[0] for j in shape.points[:]]
                    y = [j[1] for j in shape.points[:]]
                    axis_object.plot(x,y, color=shapecolors[i], linewidth=linewidths[i],
                                     zorder=zorders[i], **kwargs)

#--------------------------------------------------------------------------------------------------#

class MultiPlot(object):
    """
    Creates a grid of massplots
    Attributes:
        nrows (int)
        ncols (int)
        figwidth
        figheight
        width_ratios
        height_ratios
        title
    Kwargs:
        Most kwargs are passed massplot.create() for the subplots, except:
            rect (list of 4 floats)
    Methods:

    """
    def __init__(self, nrows, ncols, figwidth=11, figheight=8.5, width_ratios=None,
                 height_ratios=None, title=None, **kwargs):
        self.nrows = nrows
        self.ncols = ncols
        self.nplots = nrows * ncols

        # Create the figure object
        self.fig = plt.figure(figsize=(figwidth, figheight))

        # Create gridspec for grid placement
        self.gs = gridspec.GridSpec(nrows, ncols, width_ratios=width_ratios,
                                    height_ratios=height_ratios)

        # Create massplot subplots
        self.subplots = []
        for i in range(0, self.nplots):
            self.subplots.append(create(fig=self.fig, subplot=self.gs[i], **kwargs))

        if title:
            self.set_title(title)

    def __getitem__(self, *args):
        """
        Takes either a key or a row & col and returns a specific subplot
        """
        total = self.nrows*self.ncols
        key = args[0]
        if isinstance(key, tuple):
            row = key[0]
            col = key[1]
            if isinstance(row, slice) or isinstance(col, slice):
                raise NotImplementedError('Slicing is not implemented in massplot multiplots')
            if row > self.nrows:
                raise IndexError('row index out of range')
            if col > self.ncols:
                raise IndexError('col index out of range')
            key = row*self.ncols + col
        else:
            if isinstance(key, slice):
                raise NotImplementedError('Slicing is not implemented in massplot multiplots')
            else:
                if key < 0:
                    key += total
                if key >= total or key < 0:
                    raise IndexError("index out of range")

        return self.subplots[key]

    def set_title(self, title, **kwargs):
        """
        Sets title for entire figure. Wrapper for fig.suptitle()
        """
        self.fig.suptitle(t=title, **kwargs)

    def set_fig_area(self, rect, **kwargs):
        """
        Sets the padding for the overall figure. Wrapper for fig.tight_layout()
        """
        self.fig.tight_layout(rect=rect, **kwargs)

    def set_all_lim(self, xlims=None, ylims=None):
        for i in range(0, self.nplots):
            if xlims is not None:
                self.subplots[i].set_xlim(xlims)
            if ylims is not None:
                self.subplots[i].set_ylim(ylims)

    def autoscale_plots(self, axis, buffer=0, min_diff=0):
        for i in range(0, self.nplots):
            self.subplots[i].autoscale_axis(axis=axis, buffer=buffer, min_diff=min_diff)

    @staticmethod
    def add_to_pdf(pdf_object, **kwargs):
        pdf_object.savefig(**kwargs)

#--------------------------------------------------------------------------------------------------#