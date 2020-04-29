import os

def create_key(template, outtype=('nii.gz','dicom'), annotation_classes=None):
    if template is None or not template:
        raise ValueError('Template must be a valid format string')
    return (template, outtype, annotation_classes)


def infotodict(seqinfo):
    """Heuristic evaluator for determining which runs belong where

    allowed template fields - follow python string module:

    item: index within category
    subject: participant id
    seqitem: run number during scanning
    subindex: sub index within group
    """
    t1 = create_key('sub-{subject}/anat/sub-{subject}_T1w')
    rest_pre = create_key('sub-{subject}/func/sub-{subject}_task-restPre_run-{item:02d}_bold')
    rest_sbref_pre = create_key('sub-{subject}/func/sub-{subject}_task-restPre_run-{item:02d}_sbref')
    rest_post = create_key('sub-{subject}/func/sub-{subject}_task-restPost_run-{item:02d}_bold')
    rest_sbref_post = create_key('sub-{subject}/func/sub-{subject}_task-restPost_run-{item:02d}_sbref')
    spott_pav = create_key('sub-{subject}/func/sub-{subject}_task-spottpav_run-{item:02d}_bold')
    spott_pav_sbref = create_key('sub-{subject}/func/sub-{subject}_task-spottpav_run-{item:02d}_sbref')
    spott_ins = create_key('sub-{subject}/func/sub-{subject}_task-spottins_run-{item:02d}_bold')
    spott_ins_sbref = create_key('sub-{subject}/func/sub-{subject}_task-spottins_run-{item:02d}_sbref')
    spott_pit = create_key('sub-{subject}/func/sub-{subject}_task-spottpit_run-{item:02d}_bold')
    spott_pit_sbref = create_key('sub-{subject}/func/sub-{subject}_task-spottpit_run-{item:02d}_sbref')
    sefmap = create_key('sub-{subject}/fmap/sub-{subject}_dir-{dir}_epi')

    info = {t1:[], rest_pre:[], rest_sbref_pre:[], rest_post:[], rest_sbref_post:[], spott_pav:[], spott_pav_sbref:[],
            spott_ins:[], spott_ins_sbref:[], spott_pit:[], spott_pit_sbref:[], sefmap:[]}

    for idx, s in enumerate(seqinfo):
        if (s.dim4 == 1) and ('t1_mprag_sag' in s.protocol_name):
            info[t1] = [s.series_id]
            
        if (s.dim4 == 673) and ('spott_pav' in s.protocol_name):
            info[spott_pav].append({'item': s.series_id})
        if (s.dim4 == 1) and ('spott_pav_SBRef' in s.series_description):
            info[spott_pav_sbref].append({'item': s.series_id})

        if (s.dim4 > 100) and ('spott_ins' in s.protocol_name):
            info[spott_ins].append({'item': s.series_id}) 
        if (s.dim4 == 1) and ('spott_ins_SBRef' in s.series_description):
            info[spott_ins_sbref].append({'item': s.series_id})

	if (s.dim4 == 483) and ('spott_pit' in s.protocol_name):
            info[spott_pit].append({'item': s.series_id})
        if (s.dim4 == 1) and ('spott_pit_SBRef' in s.series_description):
            info[spott_pit_sbref].append({'item': s.series_id})

        if (s.dim4 > 600) and ('rest-pre' in s.protocol_name):
            info[rest_pre].append({'item': s.series_id})
        if (s.dim4 == 1) and ('rest-pre' in s.series_description):
            info[rest_sbref_pre].append({'item': s.series_id})

        if (s.dim4 > 600) and ('rest-post' in s.protocol_name):
            info[rest_post].append({'item': s.series_id})
        if (s.dim4 == 1) and ('rest-post' in s.series_description):
            info[rest_sbref_post].append({'item': s.series_id})

        if (s.dim4 == 5) and ('cmrr_mbep2d_se_1_PA' in s.protocol_name):
            info[sefmap].append({'item': s.series_id, 'dir': 'PA'})
        if (s.dim4 == 5) and ('cmrr_mbep2d_se_1_AP' in s.protocol_name):
            info[sefmap].append({'item': s.series_id, 'dir': 'AP'})


    return info
