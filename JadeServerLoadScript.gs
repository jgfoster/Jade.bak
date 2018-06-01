| loadedProject spec  |
loadedProject := Rowan image loadedProjectNamed: 'Rowan'.
spec := loadedProject specification.
spec imageSpec loadedGroupNames add: 'jadeServer'.

Rowan projectTools load loadProjectNamed: 'Rowan'