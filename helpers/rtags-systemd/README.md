** Integration with =systemd= /(GNU Linux)/

On GNU/Linux distributions based on the =systemd= service manager, =rdm= can
also be socket acivated.

 1. Add the following to =~/.config/systemd/user/rdm.socket=

   #+BEGIN_EXAMPLE
   [Unit]
   Description=RTags daemon socket

   [Socket]
   ListenStream=%t/rdm.socket

   [Install]
   WantedBy=default.target
   #+END_EXAMPLE

 2. Add the following to =~/.config/systemd/user/rdm.service=

   #+BEGIN_EXAMPLE
   [Unit]
   Description=RTags daemon

   Requires=rdm.socket

   [Service]
   Type=simple
   ExecStart=$RDM -v --inactivity-timeout 300 --log-flush
   #+END_EXAMPLE

 3. Replace =$RDM= with the path to your copy of =rdm=, and add any command
    line parameters you might usually use.

    You have to use absolute paths here. =%h= is expanded to your home
    directory.  Environment variables are not expanded inside strings.

 4. Run the following command from the terminal:

    #+BEGIN_SRC sh
    systemctl --user enable rdm.socket
    systemctl --user start rdm.socket
    #+END_SRC

    =Systemd= will create the =rdm= socket automatically.
* Usage

Now that your files are indexed you can start using RTags. Normally you
would do this from your editor but the way to extract this information from
=rdm= is to use the command line tool =rc=.
