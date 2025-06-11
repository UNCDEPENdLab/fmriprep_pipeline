import nibabel as nib
import re
from templateflow import api
from nilearn.image import resample_to_img

# Helper function to get the voxel resolution from a NIfTI file
def get_voxel_resolution(img):
    zooms = img.header.get_zooms()[:3]
    res = tuple(round(z, 3) for z in zooms)
    return res

# Helper function to parse the image space from the filename
def parse_space_from_filename(filename):
    match = re.search(r"space-([a-zA-Z0-9]+)", filename)
    return match.group(1) if match else "T1w"

# Fetch the template image from TemplateFlow
def fetch_template_image(template, resolution, suffix, desc=None, extension=".nii.gz"):
    print(f"Fetching from TemplateFlow: template={template}, desc={desc}, resolution={resolution}, suffix={suffix}")
    return api.get(
        template=template,
        desc=desc,
        resolution=resolution,
        suffix=suffix,
        extension=extension
    )

def resample_template_to_bold(in_file, output, template_resolution=1, template_space="MNI152NLin2009cAsym",
    suffix="mask", desc="brain", extension=".nii.gz", interpolation="nearest"):
    
    space = parse_space_from_filename(in_file)
    
    bold_img = nib.load(in_file)
    resolution = get_voxel_resolution(bold_img)
    print(f"Detected space: {space}, resolution: {resolution}")

    # grab the template image from templateflow
    image_path = fetch_template_image(
        template=template_space,
        resolution=template_resolution,
        suffix=suffix,
        desc=desc,
        extension=extension
    )

    template_img = nib.load(image_path)
    resampled_mask = resample_to_img(template_img, bold_img, interpolation=interpolation)
    nib.save(resampled_mask, output)
    return output
