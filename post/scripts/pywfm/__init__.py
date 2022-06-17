import os

DLL_PATH = '/libs/lib'
DLL = 'libIWFMLib.so'

from pywfm.model import IWFMModel
from pywfm.budget import IWFMBudget
from pywfm.zbudget import IWFMZBudget
from pywfm.decorators import program_timer
