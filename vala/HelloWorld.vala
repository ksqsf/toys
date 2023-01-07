using Gtk;

int main(string[] argv) {
	var app = new Gtk.Application("com.example.GtkApplication",
								  GLib.ApplicationFlags.FLAGS_NONE
								  );
	app.activate.connect(() => {
			var window = new Gtk.ApplicationWindow(app);
			var button = new Gtk.Button.with_label("Hello, world!");
			button.clicked.connect(() => {
					window.close();
				});
			window.set_child(button);
			window.present();
		});
	return app.run();
}
